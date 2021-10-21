{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PokerSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Property, forAll, property, withDiscards, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Poker.Game.Game (haveAllPlayersActed)
import Poker.Game.Utils (getActivePlayers)
import Poker.Generators (allPStates, genGame)
import Poker.Poker (canProgressGame)
import Poker.Types
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

player1 :: Player
player1 =
  Player
    { _pockets =
        Just $
          PocketCards
            Card {rank = Three, suit = Diamonds}
            Card {rank = Four, suit = Spades},
      _chips = 2000,
      _bet = 50,
      _playerState = SatIn NotFolded,
      _playerName = "player1",
      _committed = 50,
      _actedThisTurn = True,
      _possibleActions = []
    }

player2 :: Player
player2 =
  Player
    { _pockets =
        Just $
          PocketCards
            Card {rank = Three, suit = Clubs}
            Card {rank = Four, suit = Hearts},
      _chips = 0,
      _bet = 0,
      _playerState = SatIn NotFolded,
      _playerName = "player2",
      _committed = 50,
      _actedThisTurn = False,
      _possibleActions = []
    }

player3 :: Player
player3 =
  Player
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerState = SatIn NotFolded,
      _playerName = "player3",
      _committed = 50,
      _actedThisTurn = False,
      _possibleActions = []
    }

player4 :: Player
player4 =
  Player
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerState = SatOut,
      _playerName = "player4",
      _committed = 0,
      _actedThisTurn = False,
      _possibleActions = []
    }

player5 :: Player
player5 =
  Player
    { _pockets =
        Just $
          PocketCards
            Card {rank = King, suit = Diamonds}
            Card {rank = Four, suit = Spades},
      _chips = 2000,
      _bet = 50,
      _playerState = SatIn NotFolded,
      _playerName = "player1",
      _committed = 50,
      _actedThisTurn = True,
      _possibleActions = []
    }

initPlayers :: [Player]
initPlayers = [player1, player2, player3]

spec :: SpecWith ()
spec = describe "Poker" $ do
  return ()