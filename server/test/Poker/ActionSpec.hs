{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.ActionSpec where

import Control.Lens (Ixed (ix), (.~), (^.), (^?))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Poker.Game.Actions
  ( call,
    check,
    foldCards,
    makeBet,
    postBlind,
    sitOut,
  )
import Poker.Game.Utils (initialDeck)
import Poker.Poker (initialGameState)
import Poker.Types
  ( Blind (Small),
    Game (_pot, _smallBlind),
    Player (..),
    PlayerState (Folded, In, SatOut),
    Street (PreDeal, PreFlop),
    actedThisTurn,
    chips,
    committed,
    currentPosToAct,
    maxBet,
    playerState,
    players,
    pot,
    street,
  )
import Test.Hspec (describe, it, shouldBe)

initialGameState' :: Game
initialGameState' = initialGameState initialDeck

player1 :: Player
player1 =
  Player
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerState = In,
      _playerName = "player1",
      _committed = 100,
      _actedThisTurn = True,
      _possibleActions = []
    }

player2 :: Player
player2 =
  Player
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerState = Folded,
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
      _playerState = In,
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
      _playerState = In,
      _playerName = "player3",
      _committed = 0,
      _actedThisTurn = False,
      _possibleActions = []
    }

player5 :: Player
player5 =
  Player
    { _pockets = Nothing,
      _chips = 4000,
      _bet = 4000,
      _playerState = In,
      _playerName = "player5",
      _committed = 4000,
      _actedThisTurn = True,
      _possibleActions = []
    }

player6 :: Player
player6 =
  Player
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 200,
      _playerState = In,
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
  describe "postBlind" $ do
    it "should update player attributes correctly" $ do
      let game =
            (street .~ PreDeal)
              . (players .~ [(committed .~ 0) player1, player3])
              $ initialGameState'
          pName = "player1"
          blind = Small
          newGame = postBlind blind pName game
          playerWhoBet = newGame ^? players . ix 0
          smallBlindValue = _smallBlind game
          expectedPlayer =
            Player
              { _pockets = Nothing,
                _chips = 2000 - smallBlindValue,
                _bet = smallBlindValue,
                _playerState = In,
                _playerName = "player1",
                _committed = smallBlindValue,
                _actedThisTurn = True,
                _possibleActions = []
              }
      playerWhoBet `shouldBe` Just expectedPlayer

    it "should add blind bet to pot" $ do
      let game =
            (street .~ PreDeal) . (players .~ [player1, player3]) $
              initialGameState'
          pName = "player1"
          blind = Small
          newGame = postBlind blind pName game
          playerWhoBet = newGame ^? players . ix 0
      _pot newGame `shouldBe` _smallBlind game

  describe "bet" $ do
    it "should update player attributes correctly" $ do
      let game =
            (street .~ PreFlop) . (players .~ [player1, player2, player3]) $
              initialGameState'
          betValue = 200
          pName = "player1"
          newGame = makeBet betValue pName game
          playerWhoBet = newGame ^? players . ix 0
          expectedPlayer =
            Player
              { _pockets = Nothing,
                _chips = 2000 - betValue,
                _bet = betValue,
                _playerState = In,
                _playerName = "player1",
                _committed = 100 + betValue,
                _actedThisTurn = True,
                _possibleActions = []
              }
      playerWhoBet `shouldBe` Just expectedPlayer

    it "should add bet amount to pot" $ do
      let game =
            (street .~ PreFlop) . (players .~ [player1, player2, player3]) $
              initialGameState'
          betValue = 200
          pName = "player1"
          newGame = makeBet betValue pName game
      (newGame ^. pot) `shouldBe` betValue

    it "should update maxBet if amount greater than current maxBet" $ do
      let game =
            (street .~ PreFlop) . (players .~ [player1, player2, player3]) $
              initialGameState'
          betValue = 200
          pName = "player1"
          newGame = makeBet betValue pName game
      (newGame ^. maxBet) `shouldBe` betValue

    it "should update player attributes correctly when bet all in" $ do
      let game =
            (street .~ PreFlop) . (players .~ [player1, player2, player3]) $
              initialGameState'
          betValue = player1 ^. chips
          pName = "player1"
          newGame = makeBet betValue pName game
          playerWhoBet = newGame ^? players . ix 0
          expectedPlayer =
            Player
              { _pockets = Nothing,
                _chips = 0,
                _bet = betValue,
                _playerState = In,
                _playerName = "player1",
                _committed = 100 + betValue,
                _actedThisTurn = True,
                _possibleActions = []
              }
      playerWhoBet `shouldBe` Just expectedPlayer

    it "should increment position to act" $ do
      let game =
            (street .~ PreFlop) . (currentPosToAct .~ pure 0)
              . (players .~ [player1, player2, player3])
              $ initialGameState'
          betValue = 200
          pName = "player1"
          newGame = makeBet betValue pName game
          newPositionToAct = newGame ^. currentPosToAct
          expectedNewPositionToAct = Just 2
      newPositionToAct `shouldBe` expectedNewPositionToAct

  describe "foldCards" $ do
    it "should update player attributes correctly" $ do
      let game =
            (street .~ PreFlop) . (players .~ [player1, player2, player3]) $
              initialGameState'
          pName = "player1"
          newGame = foldCards pName game
          playerWhoFolded = newGame ^? players . ix 0
          expectedPlayer =
            Player
              { _pockets = Nothing,
                _chips = 2000,
                _bet = 0,
                _playerState = Folded,
                _playerName = "player1",
                _committed = 100,
                _actedThisTurn = True,
                _possibleActions = []
              }
      playerWhoFolded `shouldBe` Just expectedPlayer
    it "should increment position to act" $ do
      let game =
            (street .~ PreFlop) . (currentPosToAct .~ pure 0)
              . (players .~ [player1, player2, player3])
              $ initialGameState'
          pName = "player1"
          newGame = foldCards pName game
          newPositionToAct = newGame ^. currentPosToAct
          expectedNewPositionToAct = Just 2
      newPositionToAct `shouldBe` expectedNewPositionToAct

  describe "call" $ do
    it "should update player attributes correctly when calling a bet" $ do
      let game =
            (street .~ PreFlop) . (maxBet .~ 400)
              . (players .~ [player1, player6])
              $ initialGameState'
          pName = "player6"
          newGame = call pName game
          playerWhoCalled = newGame ^? players . ix 1
          expectedPlayer =
            Player
              { _pockets = Nothing,
                _chips = 1800,
                _bet = 400,
                _playerState = In,
                _playerName = "player6",
                _committed = 450,
                _actedThisTurn = True,
                _possibleActions = []
              }
      playerWhoCalled `shouldBe` Just expectedPlayer

    it "should update player attributes correctly when calling AllIn" $ do
      let game' =
            (street .~ PreFlop) . (maxBet .~ 4000)
              . (players .~ [player5, player1])
              $ initialGameState'
          pName' = "player1"
          newGame' = call pName' game'
          playerWhoCalled' = newGame' ^? players . ix 1
          expectedPlayer' =
            Player
              { _pockets = Nothing,
                _chips = 0,
                _bet = 2000,
                _playerState = In,
                _playerName = "player1",
                _committed = 2100,
                _actedThisTurn = True,
                _possibleActions = []
              }
      playerWhoCalled' `shouldBe` Just expectedPlayer'

    it "should increment position to act" $ do
      let game =
            (street .~ PreFlop) . (currentPosToAct .~ pure 0)
              . (players .~ [player1, player2, player3])
              $ initialGameState'
          pName = "player1"
          newGame = call pName game
          newPositionToAct = newGame ^. currentPosToAct
          expectedNewPositionToAct = Just 2
      newPositionToAct `shouldBe` expectedNewPositionToAct

  describe "check" $ do
    it "should update player attributes correctly" $ do
      let game =
            (street .~ PreFlop) . (players .~ [player1, player2]) $
              initialGameState'
          pName = "player1"
          expectedPlayers = [player1, player2, player3]
          newGame = check pName game
          playerWhoChecked = newGame ^? players . ix 0
          expectedPlayer = (actedThisTurn .~ True) player1
      playerWhoChecked `shouldBe` Just expectedPlayer

    it "should increment position to act" $ do
      let game =
            (street .~ PreFlop) . (currentPosToAct .~ pure 0)
              . (players .~ [player1, player3])
              $ initialGameState'
          pName = "player1"
          newGame = check pName game
          newPositionToAct = newGame ^. currentPosToAct
          expectedNewPositionToAct = Just 1
      newPositionToAct `shouldBe` expectedNewPositionToAct

  describe "SitOut" $
    it "should set playerState to SatOut" $ do
      let game =
            (street .~ PreDeal) . (players .~ [player1, player6]) $
              initialGameState'
          pName = "player6"
          expectedPlayer = (playerState .~ SatOut) player6
          newGame = sitOut pName game
          playerWhoChecked = newGame ^? players . ix 1
      playerWhoChecked `shouldBe` Just expectedPlayer
