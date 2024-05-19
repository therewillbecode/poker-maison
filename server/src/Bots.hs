{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Bots where

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
  ( TChan,
    TVar,
    atomically,
    dupTChan,
    readTChan,
    readTVarIO,
  )
import Control.Concurrent.STM.TChan (TChan, dupTChan, readTChan)
import Control.Exception ()
import Control.Lens ((^.))
import Control.Monad
  ( Monad (return, (>>)),
    forever,
    mapM_,
    unless,
    when,
  )
import Control.Monad.Except
  ( Monad (return, (>>)),
    MonadIO (liftIO),
    forever,
    mapM_,
    unless,
    when,
  )
import Control.Monad.Reader
  ( Monad (return, (>>)),
    MonadIO (liftIO),
    forever,
    mapM_,
    unless,
    when,
  )
import Control.Monad.STM (atomically)
import Control.Monad.State.Lazy
  ( Monad (return, (>>)),
    MonadIO (liftIO),
    forever,
    mapM_,
    unless,
    when,
  )
import Data.Either (Either (Left, Right), isRight)
import Data.Foldable (Foldable (length, null), mapM_)
import Data.Functor ((<$>))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Database (dbDepositChipsIntoPlay)
import Database.Persist.Postgresql (ConnectionString)
import qualified Network.WebSockets as WS
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Poker.ActionValidation (validateAction)
import Poker.Game.Blinds (blindRequiredByPlayer)
import Poker.Game.Game (doesPlayerHaveToAct, initPlayer)
import Poker.Game.Utils (getGamePlayer)
import Poker.Poker (initPlayer, runPlayerAction)
import Poker.Types
  ( Action (Bet, Call, Check, Fold, PostBlind, Raise, SitDown),
    Blind (Big, NoBlind, Small),
    Game (..),
    Player (..),
    PlayerAction (..),
    PlayerName,
    Street (PreDeal),
    chips,
  )
import Socket.Table (toGameInMailbox, updateTable')
import Socket.Types
  ( Err (GameErr),
    MsgOut (NewGameState),
    ServerState (..),
    Table
      ( Table,
        channel,
        game,
        gameInMailbox,
        gameOutMailbox,
        subscribers,
        waitlist
      ),
  )
import Socket.Utils (unLobby)
import System.Random (randomRIO)
import Text.Pretty.Simple (pPrint)
import Types ()
import Prelude

delayThenSeatPlayer ::
  ConnectionString -> Int -> TVar ServerState -> Player -> IO ()
delayThenSeatPlayer dbConn delayDuration s p = do
  _ <- threadDelay delayDuration
  sitDownBot dbConn p s

bot1 :: Player
bot1 = initPlayer "1@1" 2000

bot2 :: Player
bot2 = initPlayer "2@2" 2000

bot3 :: Player
bot3 = initPlayer "3@3" 2000

bot4 :: Player
bot4 = initPlayer "101@101" 2000

bot5 :: Player
bot5 = initPlayer "102@102" 2000

startBotActionLoops ::
  ConnectionString -> TVar ServerState -> Int -> [PlayerName] -> IO ()
startBotActionLoops db s playersToWaitFor botNames = do
  let botsDelayMicroseconds = 2500000
  threadDelay  botsDelayMicroseconds --delay so bots dont start game until all of them sat down
  ServerState {..} <- readTVarIO s
  case M.lookup tableName $ unLobby lobby of
    Nothing -> error "TableDoesNotExist "
    Just table@Table {..} -> do
      mapM_ (botActionLoop db s gameOutMailbox playersToWaitFor) botNames
  where
    tableName = "Black"

botActionLoop ::
  ConnectionString ->
  TVar ServerState ->
  Input Game ->
  Int ->
  PlayerName ->
  IO ThreadId
botActionLoop dbConn s gameOutMailbox playersToWaitFor botName = forkIO $
  forever $ do
    runEffect $
      fromInput gameOutMailbox
        >-> do
          g <- await
          liftIO $
            if canStartGame g
              then runBotAction dbConn s g botName
              else actIfNeeded g botName
  where
    canStartGame Game {..} =
      _street == PreDeal && (length _players >= playersToWaitFor)
    actIfNeeded g' pName' =
      let hasToAct = doesPlayerHaveToAct pName' g'
       in when (hasToAct || (blindRequiredByPlayer g' pName' /= NoBlind)) $ do
            print hasToAct
            runBotAction dbConn s g' pName'

runBotAction ::
  ConnectionString -> TVar ServerState -> Game -> PlayerName -> IO ()
runBotAction dbConn serverStateTVar g pName = do
  maybeAction <- getValidBotAction g pName
  case maybeAction of
    Nothing -> return ()
    Just a -> do
      let eitherNewGame = runPlayerAction g a
      case eitherNewGame of
        Left gameErr -> print (show $ GameErr gameErr) >> return ()
        Right g -> do
          liftIO $ async $ toGameInMailbox serverStateTVar tableName g
          liftIO $ atomically $ updateTable' serverStateTVar tableName g
  where
    tableName = "Black"
    chipsToSit = 2000

sitDownBot :: ConnectionString -> Player -> TVar ServerState -> IO ()
sitDownBot dbConn player@Player {..} serverStateTVar = do
  s@ServerState {..} <- readTVarIO serverStateTVar
  let gameMove = SitDown player
  case M.lookup tableName $ unLobby lobby of
    Nothing -> error "table doesnt exist" >> return ()
    Just Table {..} -> do
      let eitherNewGame = runPlayerAction game takeSeatAction
      case eitherNewGame of
        Left gameErr -> print $ GameErr gameErr
        Right g -> do
          dbDepositChipsIntoPlay dbConn _playerName chipsToSit
          liftIO $ async $ toGameInMailbox serverStateTVar tableName g
          liftIO $ atomically $ updateTable' serverStateTVar tableName g
  where
    chipsToSit = 2000
    tableName = "Black"
    takeSeatAction = PlayerAction {name = _playerName, action = SitDown player}

getValidBotAction :: Game -> PlayerName -> IO (Maybe PlayerAction)
getValidBotAction g@Game {..} name
  | length _players < 2 = return Nothing
  | _street == PreDeal = return $ case blindRequiredByPlayer g name of
    Small -> Just $ PlayerAction {action = PostBlind Small, ..}
    Big -> Just $ PlayerAction {action = PostBlind Big, ..}
    NoBlind -> Nothing
  | otherwise = do
    betAmount' <- randomRIO (lowerBetBound, chipCount)
    let possibleActions = actions _street betAmount'
    let actionsValidated = validateAction g name <$> possibleActions
    let pNameActionPairs = zip possibleActions actionsValidated
    let validActions = (<$>) fst $ filter (isRight . snd) pNameActionPairs
    when (null validActions) panic
    randIx <- randomRIO (0, length validActions - 1)
    return $ Just $ PlayerAction {action = validActions !! randIx, ..}
  where
    actions :: Street -> Int -> [Action]
    actions st chips
      | st == PreDeal = [PostBlind Big, PostBlind Small]
      | otherwise = [Check, Call, Fold, Bet chips, Raise chips]
    lowerBetBound = if (_maxBet > 0) then 2 * _maxBet else _bigBlind
    chipCount = maybe 0 (^. chips) (getGamePlayer g name)
    panic = do
      print $ "No valid actions for " <> show name