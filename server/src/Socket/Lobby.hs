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
import Pipes.Concurrent (atomically, newest, spawn)
import Poker.Game.Utils (shuffledDeck)
import Poker.Poker (newPreHandPlayer, initialGameState)
import Poker.Types (Game (..), unChips)
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
  (output, input) <- spawn $ newest 1
  let tableName = "Black"
  let table' =
        Table
          { subscribers = [],
            gameInMailbox = output,
            gameOutMailbox = input,
            waitlist = [],
            game = initialGameState shuffledDeck',
            channel = chan,
            config = headsUpBotsConfig
          }
  return $ Lobby $ M.fromList [("Black", table')]

joinGame :: Username -> Int -> Game -> Game
joinGame (Username username) chips Game {..} =
  Game {_players = _players <> pure player, ..}
  where
    player = newPreHandPlayer username chips

joinTableWaitlist :: Username -> Table -> Table
joinTableWaitlist username Table {..} =
  Table {waitlist = waitlist <> [username], ..}

insertTable :: TableName -> Table -> Lobby -> Lobby
insertTable tableName newTable = Lobby . (at tableName ?~ newTable) . unLobby

-- to do - return an either as there are multiple errs for why plyr cant join game ie no chips
canJoinGame :: Game -> Bool
canJoinGame Game {..} = length _players < _maxPlayers

summariseGame :: TableName -> Table -> TableSummary
summariseGame tableName Table {game = Game {..}, ..} =
  TableSummary
    { _tableName = tableName,
      _playerCount = length _players,
      _waitlistCount = length _waitlist,
      _minBuyInChips = unChips _minBuyInChips,
      _maxBuyInChips = unChips _maxBuyInChips,
      ..
    }

summariseTables :: Lobby -> [TableSummary]
summariseTables (Lobby lobby) = uncurry summariseGame <$> M.toList lobby
