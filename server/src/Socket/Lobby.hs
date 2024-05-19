{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Lobby where

import Control.Concurrent
  ( MVar,
    modifyMVar,
    modifyMVar_,
    readMVar,
  )
import Control.Concurrent.STM (atomically, newBroadcastTChan)
import Control.Concurrent.STM.TChan (newBroadcastTChan)
import Control.Lens (At (at), (.~), (?~))
import Control.Lens.At (At (at))
import Control.Monad (void)
import Control.Monad.STM (atomically)
import Data.ByteString.Char8
  ( pack,
    unpack,
  )
import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import Pipes.Concurrent (atomically, newest,latest, spawn)
import Poker.Game.Utils (shuffledDeck)
import Poker.Poker (initPlayer, initialGameState)
import Poker.Types (Game (..))
import Socket.Types
  ( Lobby (..),
    Table (..),
    TableName,
    TableSummary (..),
    headsUpBotsConfig,
  )
import Socket.Utils (unLobby)
import System.Random (getStdGen)
import Types (Username (..))

initialLobby :: IO Lobby
initialLobby = do
  chan <- atomically newBroadcastTChan
  randGen <- getStdGen
  let shuffledDeck' = shuffledDeck randGen
  let initGame = initialGameState shuffledDeck'
  (output, input) <- spawn $ newest 1 -- latest initGame
  let tableName = "Black"
  let table' =
        Table
          { subscribers = [],
            gameInMailbox = output,
            gameOutMailbox = input,
            waitlist = [],
            game = initGame,
            channel = chan,
            config = headsUpBotsConfig
          }
  return $ Lobby $ M.fromList [("Black", table')]

joinGame :: Username -> Int -> Game -> Game
joinGame (Username username) chips Game {..} =
  Game {_players = _players <> pure player, ..}
  where
    player = initPlayer username chips

joinTableWaitlist :: Username -> Table -> Table
joinTableWaitlist username Table {..} =
  Table {waitlist = waitlist <> [username], ..}

insertTable :: TableName -> Table -> Lobby -> Lobby
insertTable tableName newTable = Lobby . (at tableName ?~ newTable) . unLobby

canJoinGame :: Game -> Bool
canJoinGame Game {..} = length _players < _maxPlayers

summariseGame :: TableName -> Table -> TableSummary
summariseGame tableName Table {game = Game {..}, ..} =
  TableSummary
    { _tableName = tableName,
      _playerCount = length _players,
      _waitlistCount = length _waitlist,
      ..
    }

summariseTables :: Lobby -> [TableSummary]
summariseTables (Lobby lobby) = uncurry summariseGame <$> M.toList lobby
