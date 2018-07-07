{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Clients where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Either
import Data.Foldable
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql (ConnectionString, entityVal)
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)
import Web.JWT (Secret)

import Auth
import qualified Data.Set as Set
import Database
import Schema
import Socket.Types
import Socket.Utils
import Types

import Data.Maybe
import System.Timeout

authClient ::
     Secret
  -> TVar ServerState
  -> ConnectionString
  -> RedisConfig
  -> (MsgHandlerConfig -> IO ())
  -> WS.Connection
  -> Token
  -> IO ()
authClient secretKey state dbConn redisConfig authMsgLoop conn token = do
  authResult <- runExceptT $ verifyToken secretKey dbConn redisConfig token
  case authResult of
    (Left err) -> sendMsg conn $ ErrMsg $ AuthFailed err
    (Right User {..}) -> do
      sendMsg conn AuthSuccess
      ServerState {..} <- readTVarIO state
      atomically $
        swapTVar
          state
          (ServerState
             { clients =
                 addClient
                   Client {conn = conn, email = userEmail}
                   username
                   clients
             , ..
             })
      print "3"
      authMsgLoop msgHandlerConfig
      where username = Username userUsername
            msgHandlerConfig =
              MsgHandlerConfig
                { serverStateTVar = state
                , username = username
                , dbConn = dbConn
                , clientConn = conn
                , redisConfig = redisConfig
                }

removeClient :: Username -> TVar ServerState -> IO ServerState
removeClient username serverStateTVar = do
  ServerState {..} <- readTVarIO serverStateTVar
  let newClients = M.delete username clients
  let newState = ServerState {clients = newClients, ..}
  atomically $ swapTVar serverStateTVar newState

clientExists :: Username -> Map Username Client -> Bool
clientExists = M.member

addClient :: Client -> Username -> Map Username Client -> Map Username Client
addClient client username = M.insert username client

getClient :: Map Username Client -> Username -> Maybe Client
getClient clients username = M.lookup username clients

broadcastAllClients :: Map Username Client -> MsgOut -> IO ()
broadcastAllClients clients msg =
  forM_ (M.elems clients) (\Client {..} -> sendMsg conn msg)

broadcastTableSubscribers :: Table -> Map Username Client -> MsgOut -> IO ()
broadcastTableSubscribers Table {..} clients msg =
  forM_ subscriberConns (\Client {..} -> sendMsg conn msg)
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
  forM_ conns (\Client {..} -> sendMsg conn msg)
  where
    conns = clients `M.restrictKeys` Set.fromList usernames
