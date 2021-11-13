{-# LANGUAGE OverloadedStrings #-}

module Poker.UtilsSpec where

import Control.Lens ((.~))
import Data.List ()
import Data.List.Lens ()
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Poker.ActionValidation ()
import Poker.Game.Utils (initialDeck, modInc)
import Poker.Poker (initialGameState)
import Poker.Types
  ( Game,
    PlayerInfo (..),
    PlayerState (..),
    SatInState (..),
    Street (PreFlop),
    players,
    street,
  )
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.Hedgehog
  ( PropertyT,
    diff,
    forAll,
    hedgehog,
    modifyMaxDiscardRatio,
    (/==),
    (===),
  )

initialGameState' :: Game
initialGameState' = initialGameState initialDeck

player1 :: PlayerInfo
player1 =
  PlayerInfo
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerStatus = SatIn NotFolded,
      _playerName = "player1",
      _committed = 100,
      _actedThisTurn = True,
      _possibleActions = []
    }

player2 :: PlayerInfo
player2 =
  PlayerInfo
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerStatus = SatIn Folded,
      _playerName = "player2",
      _committed = 50,
      _actedThisTurn = False,
      _possibleActions = []
    }

player3 :: PlayerInfo
player3 =
  PlayerInfo
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerStatus = SatIn NotFolded,
      _playerName = "player3",
      _committed = 50,
      _actedThisTurn = False,
      _possibleActions = []
    }

player4 :: PlayerInfo
player4 =
  PlayerInfo
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerStatus = SatIn NotFolded,
      _playerName = "player3",
      _committed = 0,
      _actedThisTurn = False,
      _possibleActions = []
    }

player5 :: PlayerInfo
player5 =
  PlayerInfo
    { _pockets = Nothing,
      _chips = 4000,
      _bet = 4000,
      _playerStatus = SatIn NotFolded,
      _playerName = "player5",
      _committed = 4000,
      _actedThisTurn = True,
      _possibleActions = []
    }

player6 :: PlayerInfo
player6 =
  PlayerInfo
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 200,
      _playerStatus = SatIn NotFolded,
      _playerName = "player6",
      _committed = 250,
      _actedThisTurn = True,
      _possibleActions = []
    }

bettingFinishedGame :: Game
bettingFinishedGame =
  ((players .~ [player1, player2]) . (street .~ PreFlop)) initialGameState'

bettingNotFinishedGame :: Game
bettingNotFinishedGame =
  ((players .~ [player1, player2, player3, player4]) . (street .~ PreFlop))
    initialGameState'

spec = do
  describe "ModInc" $ do
    it "should increment in modulo fashion" $ do
      modInc 1 0 2 `shouldBe` 1
      modInc 1 1 1 `shouldBe` 0
      modInc 1 6 7 `shouldBe` 7

    it "result should always be greater than zero" $ do
      hedgehog $ do
        i <- forAll $ Gen.int $ Range.linear 0 9
        (modInc 1 i 9 >= 0) === True
