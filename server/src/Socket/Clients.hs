{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Clients where

import Control.Concurrent.Async (async)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    readTVar,
    readTVarIO,
    swapTVar,
  )
import Control.Lens (At (at), (^.))
import Control.Monad (Monad (return), forM_, void)
import Control.Monad.Except (MonadIO (liftIO), runExceptT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (Maybe (Just))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql
  ( ConnectionString,
    entityVal,
  )
import qualified Network.WebSockets as WS
import Pipes (each, for, runEffect, yield, (>->))
import Pipes.Concurrent (toOutput)
import Pipes.Core (push)
import Poker.Game.Privacy
  ( excludeOtherPlayerCards,
    excludePrivateCards,
  )
import Servant.Auth.Server (FromJWT (decodeJWT))
import Socket.Auth (verifyJWT)
import Socket.Types
  ( Client (..),
    Err (AuthFailed),
    Lobby (..),
    MsgIn,
    MsgOut
      ( NewGameState,
        SuccessfullySatDown,
        SuccessfullySubscribedToTable
      ),
    ServerState (..),
    Table (..),
    TableName,
    Token (..),
  )
import Socket.Utils (encodeMsgToJSON, encodeMsgX)
import Types (RedisConfig, Username (..))
import Prelude

authClient ::
  BS.ByteString ->
  TVar ServerState ->
  ConnectionString ->
  RedisConfig ->
  WS.Connection ->
  Token ->
  IO (Either Err Username)
authClient secretKey state dbConn redisConfig conn (Token token) = do
  authResult <- runExceptT $ liftIO $ verifyJWT secretKey token'
  case authResult of
    Left err -> return $ Left $ AuthFailed err
    Right (Left err) -> return $ Left $ AuthFailed $ T.pack $ show err
    Right (Right claimsSet) -> case decodeJWT claimsSet of
      Left jwtErr -> return $ Left $ AuthFailed $ T.pack $ show jwtErr
      Right username@(Username name) -> return $ pure $ Username name
  where
    token' = C.pack $ T.unpack token

removeClient :: Username -> TVar ServerState -> IO ServerState
removeClient username serverStateTVar = do
  ServerState {..} <- readTVarIO serverStateTVar
  let newClients = M.delete username clients
      newState = ServerState {clients = newClients, ..}
  atomically $ swapTVar serverStateTVar newState

clientExists :: Username -> Map Username Client -> Bool
clientExists = M.member

insertClient :: Client -> Username -> Map Username Client -> Map Username Client
insertClient client username = M.insert username client

addClient :: TVar ServerState -> Client -> STM ServerState
addClient s c@Client {..} = do
  ServerState {..} <- readTVar s
  swapTVar
    s
    ( ServerState
        { clients = insertClient c (Username clientUsername) clients,
          ..
        }
    )

getClient :: Map Username Client -> Username -> Maybe Client
getClient cs username = cs ^. at username

broadcastAllClients :: Map Username Client -> MsgOut -> IO ()
broadcastAllClients clients msg =
  forM_ (M.elems clients) (\Client {..} -> sendMsg conn msg)

broadcastTableSubscribers :: Table -> Map Username Client -> MsgOut -> IO ()
broadcastTableSubscribers Table {..} clients msg =
  forM_
    subscriberConns
    (\Client {..} -> sendMsg conn msg)
  where
    subscriberConns = clients `M.restrictKeys` Set.fromList subscribers

sendMsgs :: [WS.Connection] -> MsgOut -> IO ()
sendMsgs conns msg = forM_ conns $ \conn -> sendMsg conn msg

sendMsg :: WS.Connection -> MsgOut -> IO ()
sendMsg conn msg = WS.sendTextData conn (encodeMsgToJSON msg)

sendMsgX :: WS.Connection -> MsgIn -> IO ()
sendMsgX conn msg = WS.sendTextData conn (encodeMsgX msg)

getClientConn :: Client -> WS.Connection
getClientConn Client {..} = conn

broadcastMsg :: Map Username Client -> [Username] -> MsgOut -> IO ()
broadcastMsg clients usernames msg =
  forM_
    conns
    (\Client {..} -> sendMsg conn $ filterPrivateGameData clientUsername msg)
  where
    conns = clients `M.restrictKeys` Set.fromList usernames

-- Filter out private data such as other players cards which is not
-- intended for the client.
filterPrivateGameData :: Text -> MsgOut -> MsgOut
filterPrivateGameData username (SuccessfullySatDown tableName game) =
  SuccessfullySatDown tableName (excludeOtherPlayerCards username game)
filterPrivateGameData username (SuccessfullySubscribedToTable tableName game) =
  SuccessfullySubscribedToTable
    tableName
    (excludePrivateCards (Just username) game)
filterPrivateGameData username (NewGameState tableName game) =
  NewGameState tableName (excludeOtherPlayerCards username game)
filterPrivateGameData _ unfilteredMsg = unfilteredMsg

getTablesUserSubscribedTo :: Client -> Lobby -> [(TableName, Table)]
getTablesUserSubscribedTo Client {..} (Lobby lobby) =
  filter
    (subscriberIncludesClient . snd)
    (M.toList lobby)
  where
    subscriberIncludesClient Table {..} =
      Username clientUsername `elem` subscribers

tablesToMsgs :: Text -> [(TableName, Table)] -> [MsgOut]
tablesToMsgs clientUsername' = (<$>) toFilteredMsg
  where
    gameToMsg (tableName, Table {..}) = NewGameState tableName game
    toFilteredMsg = filterPrivateGameData clientUsername' . gameToMsg

getSubscribedGameStates :: Client -> Lobby -> [MsgOut]
getSubscribedGameStates c@Client {..} l =
  tablesToMsgs clientUsername $ getTablesUserSubscribedTo c l

-- Used so that reconnected users can get up to speed on games when they regain connection
-- after a disconnect.
updateWithLatestGames :: Client -> Lobby -> IO ()
updateWithLatestGames client@Client {..} lobby =
  void $
    async $
      runEffect $
        for
          (each latestGameStates)
          (\msg -> yield msg >-> toOutput outgoingMailbox)
  where
    latestGameStates = getSubscribedGameStates client lobby
