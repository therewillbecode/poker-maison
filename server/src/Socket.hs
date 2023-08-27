{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Socket
  ( runSocketServer,
  )
where

import Bots
import Bots (bot1, bot2)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Exception ()
import Control.Lens ((^.))
import Control.Monad (Monad (return), forever, void)
import Control.Monad.Except
  ( Monad (return),
    MonadIO (liftIO),
    MonadTrans (lift),
    forever,
    void,
  )
import Control.Monad.Reader
  ( Monad (return),
    MonadIO (liftIO),
    MonadTrans (lift),
    ReaderT (runReaderT),
    forever,
    void,
  )
import Control.Monad.STM (STM, atomically)
import Crypto.JWT ()
import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.ByteString.Lazy
  ( fromStrict,
    toStrict,
  )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.UTF8 (fromString)
import Data.Either (Either (Left, Right))
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import Database ()
import Database.Persist.Postgresql (ConnectionString)
import qualified GHC.IO.Exception as G
import qualified Network.WebSockets as WS
import Pipes
  ( MonadIO (liftIO),
    MonadTrans (lift),
    Pipe,
    Producer,
    await,
    for,
    runEffect,
    void,
    yield,
    (>->),
  )
import Pipes.Aeson (decode)
import Pipes.Concurrent
  ( Input,
    Output (send),
    STM,
    atomically,
    forkIO,
    fromInput,
    newTVarIO,
    newest,
    readTVar,
    spawn,
    toOutput,
  )
import Pipes.Core (push)
import Pipes.Parse
  ( Producer,
    StateT (runStateT),
    draw,
    lift,
    yield,
  )
import qualified Pipes.Prelude as P
import Poker.ActionValidation ()
import Poker.Game.Blinds ()
import Poker.Game.Game ()
import Poker.Game.Utils ()
import Poker.Poker ()
import Poker.Types (Game, playerName)
import Socket.Clients
  ( addClient,
    authClient,
    sendMsg,
    updateWithLatestGames,
  )
import Socket.Lobby (initialLobby, summariseTables)
import Socket.Msg (msgHandler)
import Socket.Setup ()
import Socket.Subscriptions ()
import Socket.Table
  ( setUpTablePipes,
    updateTableAndGetMailbox,
    updateTableGame,
  )
import Socket.Types
  ( Client (Client, clientUsername, conn, outgoingMailbox),
    GameMsgIn (GameMove),
    Lobby,
    MsgHandlerConfig (..),
    MsgIn,
    MsgOut (AuthSuccess, ErrMsg, NewGameState, TableList),
    ServerState (..),
    TableName,
    Token (Token),
  )
import Socket.Utils (encodeMsgToJSON, unLobby)
import Socket.Workers (forkBackgroundJobs)
import System.Random ()
import System.Timeout ()
import Types (RedisConfig, Username (Username))
import Web.JWT (Secret)
import Prelude

initialServerState :: Lobby -> ServerState
initialServerState lobby = ServerState {clients = M.empty, lobby = lobby}

-- Create the initial lobby holding all game state and then fork a new thread for each table in the lobby
-- to write new game states to the DB
runSocketServer ::
  BS.ByteString -> Int -> ConnectionString -> RedisConfig -> IO ()
runSocketServer secretKey port connString redisConfig = do
  lobby <- initialLobby
  serverStateTVar <- newTVarIO (initialServerState lobby)
  -- set up pipelines for broadcasting, progressing and logging new game states
  traverse_
    (uncurry $ setUpTablePipes connString serverStateTVar)
    (M.toList $ unLobby lobby)
  -- workers for refilling chips
  forkBackgroundJobs connString serverStateTVar lobby
  print $ "Socket server listening on " ++ (show port :: String)
  _ <-
    async $
      WS.runServer "0.0.0.0" port $
        application
          secretKey
          connString
          redisConfig
          serverStateTVar
  return ()
  where
    botNames = (^. playerName) <$> [bot1]
    playersToWaitFor = 2

-- subscriptions are handled by combining each subscribers mailbox into one large mailbox
-- where mew MsgOuts with new game states are posted
--
-- The new game state msgs will then propogate to to the subscribers mailbox and
-- sent via their websocket connection automatically
subscribeToTable :: Output MsgOut -> Output MsgOut -> Output MsgOut
subscribeToTable tableOutput playerOutput = tableOutput <> playerOutput

-- Note this doesn't propagate new game state to clients just updates the game in the lobby
updateGame :: TVar ServerState -> TableName -> Game -> STM ()
updateGame s tableName g = do
  ServerState {..} <- readTVar s
  let newLobby = updateTableGame tableName g lobby
  writeTVar s ServerState {lobby = newLobby, ..}

-- creates a mailbox which has both an input sink and output source which
-- models the bidirectionality of websockets.
-- We return input source which emits our received socket msgs.
websocketInMailbox :: MsgHandlerConfig -> IO (Output MsgIn, Output MsgOut)
websocketInMailbox conf@MsgHandlerConfig {..} = do
  (writeMsgInSource, readMsgInSource) <- spawn $ newest 1
  (writeMsgOutSource, readMsgOutSource) <- spawn $ newest 1
  async $
    forever $
      runEffect $
        fromInput readMsgInSource
          >-> msgInHandler conf
          >-> toOutput writeMsgOutSource -- process received MsgIn's and place resulting MsgOut in outgoing mailbox
  async $ socketMsgOutWriter clientConn readMsgOutSource -- send encoded MsgOuts from outgoing mailbox to socket
  return (writeMsgInSource, writeMsgOutSource)

-- Runs an IO action forever which parses read MsgIn's from the websocket connection
-- and puts them in our mailbox waiting to be processed by our MsgIn handler
--
--  Note - only parsed MsgIns make it into the mailbox - socket msgs which cannot be parsed
-- are silently ignored but logged anyway.
socketMsgInWriter :: WS.Connection -> Output MsgIn -> IO ()
socketMsgInWriter conn writeMsgInSource = do
  _ <-
    async $
      forever $
        runEffect $
          msgInDecoder (socketReader conn >-> logMsgIn)
            >-> toOutput writeMsgInSource
  return ()

socketMsgOutWriter :: WS.Connection -> Input MsgOut -> IO (Async ())
socketMsgOutWriter conn is =
  forever $
    runEffect $
      for
        (fromInput is >-> msgOutEncoder)
        (lift . WS.sendTextData conn)

-- Converts a websocket connection into a producer
socketReader :: WS.Connection -> Producer BS.ByteString IO ()
socketReader conn = forever $ do
  msg <- liftIO $ WS.receiveData conn
  yield msg

-- Convert a raw Bytestring producer of raw JSON into a new producer which yields
-- only successfully parsed values of type MsgIn.
--
-- Note that this parser deliberately ignores parsing errors as the naive implementation
-- would lead to parse errors closing the stream pipeline and thus the socket connection
msgInDecoder :: Producer BS.ByteString IO () -> Producer MsgIn IO ()
msgInDecoder rawMsgProducer = do
  (x, p') <- lift $ runStateT decode rawMsgProducer
  case x of
    Nothing -> return ()
    Just (Left a) -> do
      (_invalidMsg, p'') <- lift $ runStateT draw p'
      msgInDecoder p''
    Just c@(Right parsedMsgIn) -> do
      yield parsedMsgIn
      msgInDecoder p'

msgOutEncoder :: Pipe MsgOut Text IO ()
msgOutEncoder = do
  msgOut <- await
  yield $ encodeMsgToJSON msgOut

-- branches of code which do not yield messages place the burden of informing the client
-- onto the table pipeline as opposed to the remaining components after the player's socket
-- pipeline. Or in other words without yielding a msg this pipe will not directly inform the client
-- about what has happened.
msgInHandler :: MsgHandlerConfig -> Pipe MsgIn MsgOut IO ()
msgInHandler conf@MsgHandlerConfig {..} = do
  msgIn <- await
  res <- lift $ runReaderT (msgHandler msgIn) conf
  case res of
    Left err -> yield $ ErrMsg err
    Right (NewGameState tableName g) ->
      liftIO $ atomically $ updateGameState serverStateTVar tableName g
    Right m -> yield m

-- The main function for handling game updates which consists of
-- a series of events whose order must be guaranteed which
-- is why they are grouped in a STM block.
--
-- 1 - We update our game in the server state
-- 2 - We send new game to
-- the table's mailbox for broadcasting to clients and other actions
-- such as progressing the game along if possible
updateGameState :: TVar ServerState -> TableName -> Game -> STM ()
updateGameState serverStateTVar tableName newGame = do
  mbGameInMailbox' <- updateTableAndGetMailbox serverStateTVar tableName newGame
  case mbGameInMailbox' of
    Nothing -> return ()
    Just gameInMailbox' -> void (send gameInMailbox' newGame)

logMsgIn :: Pipe BS.ByteString BS.ByteString IO ()
logMsgIn = do
  msg <- await
  yield msg

logMsgOut :: Pipe MsgOut MsgOut IO ()
logMsgOut = do
  msg <- await
  yield msg


-- get a pipe which only forwards the game moves which occur at the given table
filterMsgsForTable :: Monad m => TableName -> Pipe GameMsgIn GameMsgIn m ()
filterMsgsForTable tableName =
  P.filter $ \(GameMove tableName' _) -> tableName == tableName'

-- New WS connections are expected to supply an access token as an initial msg
-- Once the token is verified the connection only then will the server state be
-- updated with the newly authenticated client.
--
-- After the client has been authenticated we fork a thread which writes
-- the clients msgs to a channel.
application ::
  BS.ByteString ->
  ConnectionString ->
  RedisConfig ->
  TVar ServerState ->
  WS.ServerApp
application secretKey dbConnString redisConfig s pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  authMsg <- WS.receiveData conn
  ServerState {..} <- readTVarIO s
  eUsername <-
    authClient
      secretKey
      s
      dbConnString
      redisConfig
      conn
      (Token authMsg)
  case eUsername of
    Right u@(Username clientUsername) -> do
      (incomingMailbox, outgoingMailbox) <- websocketInMailbox $ msgConf conn u
      let client = Client {..}
      sendMsg conn AuthSuccess
      let isReconnect = client `elem` clients -- if client already on our list of clients then this is a reconnect
      updateWithLatestGames client lobby -- Sync game state with reconnected clients
      let tableSummaries = TableList $ summariseTables lobby
      liftIO $ sendMsg conn tableSummaries
      atomically $ addClient s client
      ServerState {..} <- liftIO $ atomically $ readTVar s
      forever $ do
        m <- WS.receiveData conn
        runEffect $
          msgInDecoder (yield m >-> logMsgIn)
            >-> toOutput incomingMailbox
        return ()
    Left err -> sendMsg conn (ErrMsg err)
  where
    msgConf c username =
      MsgHandlerConfig
        { serverStateTVar = s,
          dbConn = dbConnString,
          clientConn = c,
          redisConfig = redisConfig,
          ..
        }
