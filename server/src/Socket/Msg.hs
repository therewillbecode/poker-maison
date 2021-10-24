{-# LANGUAGE LambdaCase #-}
-- TODO - Factor out repetitive STM actions that lookup table and throw stm error if not found
-- with monadic composition
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Socket.Msg where

import Control.Applicative (Applicative ((<*)), (<$>))
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Monad (Monad (return))
import Control.Monad.Except (Monad (return), MonadIO (liftIO))
import Control.Monad.Reader
  ( Monad (return),
    MonadIO (liftIO),
    MonadReader (ask),
    ReaderT,
  )
import Control.Monad.STM (atomically)
import Control.Monad.State.Lazy (Monad (return), MonadIO (liftIO))
import Data.Either (Either (..), either)
import Data.Foldable (find, notElem)
import Data.Functor ((<$>))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text (Text)
import qualified Data.Text as T
import Database
  ( dbDepositChipsIntoPlay,
    dbGetUserByUsername,
    dbWithdrawChipsFromPlay,
  )
import Database.Persist.Postgresql (ConnectionString)
import qualified Network.WebSockets as WS
import Poker.Game.Game (initPlayer)
import Poker.Game.Utils (getGamePlayerNames)
import Poker.Poker (initPlayer, runPlayerAction)
import Poker.Types
import Schema
  ( UserEntity
      ( UserEntity,
        userEntityAvailableChips,
        userEntityChipsInPlay,
        userEntityCreatedAt,
        userEntityEmail,
        userEntityPassword,
        userEntityUsername
      ),
  )
import Socket.Clients (sendMsg)
import Socket.Lobby (summariseTables)
import Socket.Subscriptions (subscribeToTableHandler)
import Socket.Table (getTable, updateTable')
import Socket.Types
  ( Err
      ( ChipAmountNotWithinBuyInRange,
        GameErr,
        NotEnoughChipsToSit,
        NotSatInGame,
        TableDoesNotExist,
        UserDoesNotExistInDB
      ),
    GameMsgIn (..),
    MsgHandlerConfig (..),
    MsgIn (GameMsgIn, GetTables, SubscribeToTable),
    MsgOut (NewGameState, SuccessfullySatDown, TableList),
    ServerState (ServerState, clients, lobby),
    Table (..),
    TableName,
  )
import Socket.Utils (unLobby)
import Text.Pretty.Simple (pPrint)
import Types (unUsername)
import Prelude

msgHandler :: MsgIn -> ReaderT MsgHandlerConfig IO (Either Err MsgOut)
msgHandler GetTables {} = getTablesHandler
msgHandler msg@SubscribeToTable {} = subscribeToTableHandler msg
msgHandler (GameMsgIn msg) = gameMsgHandler msg

gameMsgHandler :: GameMsgIn -> ReaderT MsgHandlerConfig IO (Either Err MsgOut)
gameMsgHandler msg@TakeSeat {} = takeSeatHandler msg
gameMsgHandler msg@LeaveSeat {} = leaveSeatHandler msg
gameMsgHandler m@(GameMove tableName action) = do
  conf@MsgHandlerConfig {..} <- ask
  let playerAction = PlayerAction {name = unUsername username, ..}
  moveResult <- liftIO $ playMove conf tableName playerAction
  return $ NewGameState tableName <$> moveResult

playMove ::
  MsgHandlerConfig -> TableName -> PlayerAction -> IO (Either Err Game)
playMove conf@MsgHandlerConfig {..} tableName playerAction = do
  maybeTable <- liftIO $ atomically $ getTable serverStateTVar tableName
  case maybeTable of
    Nothing -> return $ Left $ TableDoesNotExist tableName
    Just Table {..} ->
      return $ either (Left . GameErr) Right $ runPlayerAction game playerAction

getTablesHandler :: ReaderT MsgHandlerConfig IO (Either Err MsgOut)
getTablesHandler = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readTVarIO serverStateTVar
  let tableSummaries = TableList $ summariseTables lobby
  liftIO $ print tableSummaries
  liftIO $ sendMsg clientConn tableSummaries
  return $ Right tableSummaries

-- We fork a new thread for each game joined to receive game updates and propagate them to the client
-- We link the new thread to the current thread so on any exception in either then both threads are
-- killed to prevent memory leaks.
--
---- If game is in predeal stage then add player to game else add to waitlist
-- the waitlist is a queue awaiting the next predeal stage of the game
takeSeatHandler :: GameMsgIn -> ReaderT MsgHandlerConfig IO (Either Err MsgOut)
takeSeatHandler (TakeSeat tableName chipsToSit) = do
  conf@MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readTVarIO serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing -> return $ Left $ TableDoesNotExist tableName
    Just table@Table {..} -> do
      canSit <- canTakeSeat chipsToSit tableName table
      case canSit of
        Left err -> return $ Left err
        Right () -> do
          let player = initPlayer (unUsername username) chipsToSit
              playerAction =
                PlayerAction
                  { name = unUsername username,
                    action = SitDown player
                  }
              takeSeatAction = GameMove tableName (SitDown player)
          case runPlayerAction
            game
            PlayerAction
              { name = unUsername username,
                action = SitDown player
              } of
            Left gameErr -> return $ Left $ GameErr gameErr
            Right newGame -> do
              liftIO $ postTakeSeat conf tableName chipsToSit
              liftIO $
                sendMsg clientConn (SuccessfullySatDown tableName newGame)
              let msgOut = NewGameState tableName newGame
              liftIO $
                atomically $
                  updateTable'
                    serverStateTVar
                    tableName
                    newGame
              return $ Right msgOut

postTakeSeat :: MsgHandlerConfig -> TableName -> Int -> IO ()
postTakeSeat conf@MsgHandlerConfig {..} name chipsSatWith =
  dbDepositChipsIntoPlay dbConn (unUsername username) chipsSatWith

leaveSeatHandler :: GameMsgIn -> ReaderT MsgHandlerConfig IO (Either Err MsgOut)
leaveSeatHandler leaveSeatMove@(LeaveSeat tableName) = do
  msgHandlerConfig@MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readTVarIO serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing -> return $ Left $ TableDoesNotExist tableName
    Just table@Table {..} ->
      if unUsername username `notElem` getGamePlayerNames game
        then return $ Left $ NotSatInGame tableName
        else do
          let eitherProgressedGame =
                runPlayerAction
                  game
                  PlayerAction {name = unUsername username, action = LeaveSeat'}

          case eitherProgressedGame of
            Left gameErr -> return $ Left $ GameErr gameErr
            Right newGame -> do
              let maybePlayer =
                    find
                      (\Player {..} -> unUsername username == _playerName)
                      (_players game)
              case maybePlayer of
                Nothing -> return $ Left $ NotSatInGame tableName
                Just Player {_chips = chipsInPlay, ..} -> do
                  liftIO $
                    dbWithdrawChipsFromPlay
                      dbConn
                      (unUsername username)
                      (unChips chipsInPlay)
                  let msgOut = NewGameState tableName newGame
                  liftIO $
                    atomically $
                      updateTable'
                        serverStateTVar
                        tableName
                        newGame
                  return $ Right msgOut

canTakeSeat ::
  Int -> Text -> Table -> ReaderT MsgHandlerConfig IO (Either Err ())
canTakeSeat chipsToSit tableName Table {game = Game {..}, ..}
  | Chips chipsToSit >= _minBuyInChips && Chips chipsToSit <= _maxBuyInChips = do
    availableChipsE <- getPlayersAvailableChips
    MsgHandlerConfig {..} <- ask
    case availableChipsE of
      Left err -> return $ Left err
      Right chips -> do
        tableE <- liftIO $ checkTableExists serverStateTVar tableName
        return $ tableE <* hasEnoughChips chips chipsToSit
  | otherwise = return $ Left $ ChipAmountNotWithinBuyInRange tableName
  where
    hasEnoughChips availableChips chipsNeeded =
      if availableChips >= chipsToSit
        then return $ Right ()
        else return $ Left NotEnoughChipsToSit
    checkTableExists s name = do
      t <- atomically $ getTable s name
      case t of
        Nothing -> return $ Left $ TableDoesNotExist name
        _ -> return $ Right ()

getPlayersAvailableChips :: ReaderT MsgHandlerConfig IO (Either Err Int)
getPlayersAvailableChips = do
  MsgHandlerConfig {..} <- ask
  maybeUser <- liftIO $ dbGetUserByUsername dbConn username
  return $ case maybeUser of
    Nothing -> Left $ UserDoesNotExistInDB (unUsername username)
    Just UserEntity {..} ->
      Right $ userEntityAvailableChips - userEntityChipsInPlay
