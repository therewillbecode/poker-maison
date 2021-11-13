{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.BlindSpec where

import Control.Lens ((.~))
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog
  ( Property,
    assert,
    forAll,
    property,
    withDiscards,
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Poker.Game.Blinds
  ( blindRequiredByPlayer,
    getRequiredBlinds,
    getSmallBlindPosition,
    haveRequiredBlindsBeenPosted,
    updatePlayersInHand,
  )
import Poker.Game.Utils (getGamePlayerNames, initialDeck)
import Poker.Generators (allPStates, genGame)
import Poker.Poker (initPlayer, initialGameState)
import Poker.Types
  ( Blind (BigBlind, NoBlind, SmallBlind),
    Deck (Deck),
    Game (..),
    PlayerInfo (..),
    PlayerState (..),
    SatInState (..),
    Street (PreDeal),
    Winners (NoWinners),
    playerStatus,
    players,
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

twoPlayerGame :: Game
twoPlayerGame =
  Game
    { _players =
        [ PlayerInfo
            { _pockets = Nothing,
              _chips = 1950,
              _bet = 50,
              _playerStatus = SatIn NotFolded,
              _playerName = "player1",
              _committed = 50,
              _actedThisTurn = False,
              _possibleActions = []
            },
          PlayerInfo
            { _pockets = Nothing,
              _chips = 2000,
              _bet = 0,
              _playerStatus = SatOut,
              _playerName = "player2",
              _committed = 0,
              _actedThisTurn = False,
              _possibleActions = []
            }
        ],
      _maxPlayers = 5,
      _board = [],
      _waitlist = [],
      _deck = Deck [],
      _smallBlind = 25,
      _bigBlind = 50,
      _street = PreDeal,
      _pot = 0,
      _minBuyInChips = 1000,
      _maxBuyInChips = 3000,
      _maxBet = 0,
      _dealer = 0,
      _currentPosToAct = Just 1,
      _winners = NoWinners
    }

twoPlayerGameAllBlindsPosted :: Game
twoPlayerGameAllBlindsPosted =
  Game
    { _players =
        [ PlayerInfo
            { _pockets = Nothing,
              _chips = 1950,
              _bet = 25,
              _playerStatus = SatIn NotFolded,
              _playerName = "player1",
              _committed = 25,
              _actedThisTurn = True,
              _possibleActions = []
            },
          PlayerInfo
            { _pockets = Nothing,
              _chips = 2000,
              _bet = 50,
              _playerStatus = SatIn NotFolded,
              _playerName = "player2",
              _committed = 50,
              _actedThisTurn = True,
              _possibleActions = []
            }
        ],
      _maxPlayers = 5,
      _board = [],
      _waitlist = [],
      _deck = Deck [],
      _smallBlind = 25,
      _bigBlind = 50,
      _street = PreDeal,
      _pot = 0,
      _winners = NoWinners,
      _maxBet = 0,
      _dealer = 0,
      _minBuyInChips = 1000,
      _maxBuyInChips = 3000,
      _currentPosToAct = Just 1
    }

threePlayerGame :: Game
threePlayerGame =
  Game
    { _players =
        [ PlayerInfo
            { _pockets = Nothing,
              _chips = 1950,
              _bet = 0,
              _playerStatus = SatOut,
              _playerName = "player1",
              _committed = 0,
              _actedThisTurn = False,
              _possibleActions = []
            },
          PlayerInfo
            { _pockets = Nothing,
              _chips = 2000,
              _bet = 0,
              _playerStatus = SatOut,
              _playerName = "player2",
              _committed = 0,
              _actedThisTurn = False,
              _possibleActions = []
            },
          PlayerInfo
            { _pockets = Nothing,
              _chips = 2000,
              _bet = 0,
              _playerStatus = SatOut,
              _playerName = "player3",
              _committed = 0,
              _actedThisTurn = False,
              _possibleActions = []
            }
        ],
      _maxPlayers = 5,
      _board = [],
      _waitlist = [],
      _deck = Deck [],
      _smallBlind = 25,
      _bigBlind = 50,
      _street = PreDeal,
      _pot = 0,
      _winners = NoWinners,
      _maxBet = 0,
      _dealer = 0,
      _minBuyInChips = 1000,
      _maxBuyInChips = 3000,
      _currentPosToAct = Just 1
    }

threePlayerGameAllBlindsPosted :: Game
threePlayerGameAllBlindsPosted =
  Game
    { _players =
        [ PlayerInfo
            { _pockets = Nothing,
              _chips = 1950,
              _bet = 0,
              _playerStatus = SatOut,
              _playerName = "player1",
              _committed = 0,
              _actedThisTurn = False,
              _possibleActions = []
            },
          PlayerInfo
            { _pockets = Nothing,
              _chips = 2000,
              _bet = 25,
              _playerStatus = SatIn NotFolded,
              _playerName = "player2",
              _committed = 25,
              _actedThisTurn = False,
              _possibleActions = []
            },
          PlayerInfo
            { _pockets = Nothing,
              _chips = 2000,
              _bet = 50,
              _playerStatus = SatIn NotFolded,
              _playerName = "player3",
              _committed = 50,
              _actedThisTurn = False,
              _possibleActions = []
            }
        ],
      _maxPlayers = 5,
      _board = [],
      _waitlist = [],
      _deck = Deck [],
      _smallBlind = 25,
      _bigBlind = 50,
      _street = PreDeal,
      _pot = 0,
      _minBuyInChips = 1000,
      _maxBuyInChips = 3000,
      _winners = NoWinners,
      _maxBet = 0,
      _dealer = 0,
      _currentPosToAct = Just 1
    }

twoPlayerNames :: [Text]
twoPlayerNames = getGamePlayerNames twoPlayerGame

twoPlayers :: [PlayerInfo]
twoPlayers = _players twoPlayerGame

threePlayerNames :: [Text]
threePlayerNames = getGamePlayerNames threePlayerGame

threePlayers :: [PlayerInfo]
threePlayers = _players threePlayerGame

prop_requiredBlinds_always_valid_arrangement_for_2_plyrs :: Property
prop_requiredBlinds_always_valid_arrangement_for_2_plyrs = withDiscards 225 . property $ do
  g <- forAll $ Gen.filter twoPlayers (genGame [PreDeal] allPStates)
  let legalBlindArrangements = [[SmallBlind, BigBlind], [SmallBlindind, SmallBlind]]
      requiredBlinds = getRequiredBlinds g
  assert (requiredBlinds `elem` legalBlindArrangements)
  where
    twoPlayers = (== 2) . length . _players

prop_requiredBlinds_always_valid_arrangement_for_3_plyrs :: Property
prop_requiredBlinds_always_valid_arrangement_for_3_plyrs = withDiscards 225 . property $ do
  g <- forAll $ Gen.filter threePlayer (genGame [PreDeal] allPStates)
  let legalBlindArrangements = [[SmallBlind, BigBlind, NoBlind], [BigBlind,SmallBlindind, SmallBlind], SmallBlindind, SmallBlind, BigBlind]]
      requiredBlinds = getRequiredBlinds g
  assert (requiredBlinds `elem` legalBlindArrangements)
  where
    threePlayer = (== 3) . length . _players

spec = do
  describe "blind required by player" $
    it "should return correct blind" $
      blindRequiredByPlayer twoPlayerGame "player2" `shouldBe` BigBlind

  describe "getSmallBlindPosition" $ do
    it "small blind position should be correct for a two player game" $ do
      let dealerPos = 0
      getSmallBlindPosition twoPlayerNames dealerPos `shouldBe` (0 :: Int)

    it "small blind position should be correct for a three player game" $ do
      let dealerPos = 2
      getSmallBlindPosition threePlayerNames dealerPos `shouldBe` (0 :: Int)

  describe "getRequiredBlinds" $ do
    it "Should return valid required blinds for two player game" $
      hedgehog $ do
        let isTwoPlayers = (== 2) . length . _players
        g <- forAll $ Gen.filter isTwoPlayers (genGame [PreDeal] allPStates)
        let legalBlindArrangements = [[SmallBlind, BigBlind], [SmallBlindind, SmallBlind]]
            requiredBlinds = getRequiredBlinds g
        --
        (requiredBlinds `elem` legalBlindArrangements) === True

    it "Should return valid required blinds for three player game" $
      hedgehog $ do
        let isThreePlayers = (== 3) . length . _players
        g <- forAll $ Gen.filter isThreePlayers (genGame [PreDeal] allPStates)
        let legalBlindArrangements = [[SmallBlind, BigBlind, NoBlind], [BigBlind,SmallBlindind, SmallBlind], SmallBlindind, SmallBlind, BigBlind]]
            requiredBlinds = getRequiredBlinds g
        (requiredBlinds `elem` legalBlindArrangements) === True

  describe "blinds" $ do
    describe "getSmallBlindPosition" $ do
      it "returns correct small blind position in three player game" $ do
        let dealerPos = 0
        getSmallBlindPosition ["Player1", "Player2", "Player3"] dealerPos
          `shouldBe` 1

      it "returns correct small blind position in two player game" $ do
        let dealerPos = 0
        getSmallBlindPosition ["Player1", "Player2"] dealerPos `shouldBe` 0

    describe "blindRequiredByPlayer" $ do
      it "returns SmallBlind if player position is dealer + 1 for three players" $ do
        let testPlayers =
              (playerStatus .~ SatIn NotFolded)
                <$> (initPlayer <$> ["Player1", "Player2", "Player3"] <*> [100])
        let game = players .~ testPlayers $ initialGameState'
        blindRequiredByPlayer game "Player2" `shouldBe` SmallBlind

      it "returns BigBlind if player position is dealer + 2 for three players" $ do
        let testPlayers =
              (playerStatus .~ SatIn NotFolded)
                <$> (initPlayer <$> ["Player1", "Player2", "Player3"] <*> [100])
        let game = players .~ testPlayers $ initialGameState'
        blindRequiredByPlayer game "Player3" `shouldBe` BigBlind

      it
        "returns NoBlind if player position is dealer for three players and playerStatus is SatIn NotFolded"
        $ do
          let testPlayers =
                (playerStatus .~ SatIn NotFolded)
                  <$> (initPlayer <$> ["Player1", "Player2", "Player3"] <*> [100])
          let game = players .~ testPlayers $ initialGameState'
          blindRequiredByPlayer game "Player1" `shouldBe` NoBlind

      it
        "returns BigBlind if player position is dealer for three players and playerStatus is SatOut"
        $ do
          let testPlayers =
                (playerStatus .~ SatOut)
                  <$> (initPlayer <$> ["Player1", "Player2", "Player3"] <*> [100])
          let game = players .~ testPlayers $ initialGameState'
          blindRequiredByPlayer game "Player1" `shouldBe` NoBlind

      it "returns SmallBlind if player position is dealer for two players" $ do
        let testPlayers =
              (playerStatus .~ SatIn NotFolded)
                <$> (initPlayer <$> ["Player1", "Player2"] <*> [100])
        let game = players .~ testPlayers $ initialGameState'
        blindRequiredByPlayer game "Player1" `shouldBe` SmallBlind

      it "returns BigBlind if player position is dealer + 1 for two players" $ do
        let testPlayers = initPlayer <$> ["Player1", "Player2"] <*> [100]
        let game = players .~ testPlayers $ initialGameState'
        blindRequiredByPlayer game "Player2" `shouldBe` BigBlind

  describe "haveRequiredBlindsBeenPosted" $ do
    it
      "should return False when not all players have posted blinds in 2 player game"
      $ haveRequiredBlindsBeenPosted twoPlayerGame `shouldBe` False

    it "should return True when all players have posted blinds in 2 player game" $
      haveRequiredBlindsBeenPosted twoPlayerGameAllBlindsPosted `shouldBe` True

    it
      "should return False when not all players have posted blinds in 3 player game"
      $ haveRequiredBlindsBeenPosted threePlayerGame `shouldBe` False

    it
      "should  return True when all players have posted blinds in 3 player game"
      $ haveRequiredBlindsBeenPosted threePlayerGameAllBlindsPosted
        `shouldBe` True

  describe "updatePlayersInHand" $ do
    it
      "should set players that are not in blind position to SatIn NotFolded for three players"
      $ do
        let newGame = updatePlayersInHand threePlayerGameAllBlindsPosted
        let playerStates = (\PlayerInfo {..} -> _playerStatus) <$> _players newGame
        playerStates `shouldBe` [SatIn NotFolded, SatIn NotFolded, SatIn NotFolded]

    it
      "should return correct player states for two players when all blinds posted"
      $ do
        let newGame = updatePlayersInHand twoPlayerGameAllBlindsPosted
        let playerStates = (\PlayerInfo {..} -> _playerStatus) <$> _players newGame
        playerStates `shouldBe` [SatIn NotFolded, SatIn NotFolded]

    it
      "should return correct player states for two players when not all blinds posted"
      $ do
        let newGame = updatePlayersInHand twoPlayerGame
        let playerStates = (\PlayerInfo {..} -> _playerStatus) <$> _players newGame
        playerStates `shouldBe` [SatIn NotFolded, SatOut]
