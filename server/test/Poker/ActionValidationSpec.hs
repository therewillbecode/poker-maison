{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.ActionValidationSpec where

import Control.Lens (element, (%~), (&), (.~), (?~))
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as T
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog (Property, forAll, property, withDiscards, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Poker.ActionValidation
  ( canBet,
    canCall,
    canCheck,
    canFold,
    canPostBlind,
    canRaise,
    canShowOrMuckHand,
    checkPlayerSatAtTable,
    isPlayerActingOutOfTurn,
    validateAction,
    validateBlindAction,
  )
import Poker.Game.Utils (initialDeck)
import Poker.Generators (allPStates, allPStreets, genGame)
import Poker.Poker (initialGameState)
import Poker.Types
  ( Action (Check, Fold, LeaveSeat', PostBlind, SitOut, Timeout),
    Blind (Big, Small),
    CurrentPlayerToActErr (CurrentPlayerToActErr),
    Deck (Deck),
    Game (..),
    GameErr (InvalidMove, NotAtTable),
    HandRank (Pair),
    InvalidMoveErr
      ( AlreadySatOut,
        BetLessThanBigBlind,
        CannotBetShouldRaiseInstead,
        CannotCallZeroAmountCheckOrBetInstead,
        CannotCheckShouldCallRaiseOrFold,
        CannotLeaveSeatOutsidePreDeal,
        CannotShowHandOrMuckHand,
        CannotSitOutOutsidePreDeal,
        InvalidActionForStreet,
        NoPlayerCanAct,
        NotEnoughChipsForAction,
        OutOfTurn
      ),
    Player
      ( Player,
        _actedThisTurn,
        _bet,
        _chips,
        _committed,
        _playerName,
        _playerState,
        _pockets,
        _possibleActions
      ),
    PlayerShowdownHand (PlayerShowdownHand),
    PlayerState (Folded, In, SatOut),
    Street (Flop, PreDeal, PreFlop, River, Showdown, Turn),
    Winners (MultiPlayerShowdown, NoWinners, SinglePlayerShowdown),
    actedThisTurn,
    bet,
    chips,
    committed,
    currentPosToAct,
    dealer,
    deck,
    maxBet,
    playerState,
    players,
    pot,
    street,
    winners,
  )
import Test.Hspec (describe, it, shouldBe)

initialGameState' :: Game
initialGameState' = initialGameState initialDeck

player1 :: Player
player1 =
  Player
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 200,
      _playerState = In,
      _playerName = "player1",
      _committed = 250,
      _actedThisTurn = False,
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
      _chips = 300,
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
      _playerState = SatOut,
      _playerName = "player4",
      _committed = 0,
      _actedThisTurn = False,
      _possibleActions = []
    }

player5 :: Player
player5 =
  Player
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerState = In,
      _playerName = "player5",
      _committed = 50,
      _actedThisTurn = False,
      _possibleActions = []
    }

playerFixtures :: [Player]
playerFixtures = [player1, player2, player3, player4]

playerFixtures2 :: [Player]
playerFixtures2 = [player3, player5]

callAllInHeadsUpFixture :: Game
callAllInHeadsUpFixture =
  Game
    { _dealer = 1,
      _currentPosToAct = Just 0,
      _smallBlind = 25,
      _bigBlind = 50,
      _minBuyInChips = 1500,
      _maxBuyInChips = 3000,
      _pot = 2500,
      _maxBet = 2400,
      _street = Turn,
      _winners = NoWinners,
      _board = [],
      _maxPlayers = 6,
      _waitlist = [],
      _deck = Deck [],
      _players =
        [ Player
            { _pockets = Nothing,
              _chips = 3500,
              _bet = 0,
              _playerState = In,
              _playerName = "player0",
              _committed = 50,
              _actedThisTurn = True,
              _possibleActions = []
            },
          Player
            { _pockets = Nothing,
              _chips = 0,
              _bet = 2400,
              _playerState = In,
              _playerName = "player1",
              _committed = 2450,
              _actedThisTurn = True,
              _possibleActions = []
            }
        ]
    }

preDealHeadsUpFixture :: Game
preDealHeadsUpFixture =
  Game
    { _dealer = 0,
      _currentPosToAct = Just 0,
      _smallBlind = 25,
      _bigBlind = 50,
      _minBuyInChips = 1500,
      _maxBuyInChips = 3000,
      _pot = 50,
      _maxBet = 50,
      _street = PreDeal,
      _winners = NoWinners,
      _board = [],
      _maxPlayers = 6,
      _waitlist = [],
      _deck = Deck [],
      _players =
        [ Player
            { _pockets = Nothing,
              _chips = 3000,
              _bet = 0,
              _playerState = In,
              _playerName = "player0",
              _committed = 0,
              _actedThisTurn = False,
              _possibleActions = []
            },
          Player
            { _pockets = Nothing,
              _chips = 2950,
              _bet = 50,
              _playerState = In,
              _playerName = "player1",
              _committed = 50,
              _actedThisTurn = True,
              _possibleActions = []
            }
        ]
    }

turnGameThreePlyrs :: Game
turnGameThreePlyrs =
  Game
    { _dealer = 2,
      _currentPosToAct = Just 0,
      _smallBlind = 25,
      _bigBlind = 50,
      _minBuyInChips = 1500,
      _maxBuyInChips = 3000,
      _pot = 550,
      _maxBet = 0,
      _street = Turn,
      _winners = NoWinners,
      _board = [],
      _maxPlayers = 6,
      _waitlist = [],
      _deck = Deck [],
      _players =
        [ Player
            { _pockets = Nothing,
              _chips = 2197,
              _bet = 0,
              _playerState = In,
              _playerName = "player0",
              _committed = 50,
              _actedThisTurn = False,
              _possibleActions = []
            },
          Player
            { _pockets = Nothing,
              _chips = 1847,
              _bet = 0,
              _playerState = In,
              _playerName = "player1",
              _committed = 250,
              _actedThisTurn = False,
              _possibleActions = []
            },
          Player
            { _pockets = Nothing,
              _chips = 2072,
              _bet = 0,
              _playerState = In,
              _playerName = "player2",
              _committed = 250,
              _actedThisTurn = False,
              _possibleActions = []
            }
        ]
    }

prop_plyrShouldntBeAbleToPostBlindsWhenNoChips :: Property
prop_plyrShouldntBeAbleToPostBlindsWhenNoChips = withDiscards 225 . property $ do
  g <- forAll $ genGame allPStreets allPStates
  blind' <- forAll $ Gen.element [Small, Big]
  let g' = g & players . element 0 %~ chips .~ 0
      action' = PostBlind blind'
      pName = "player0"
  isLeft (validateAction g' pName action') === True
  where
    actionStages = [PreDeal]

prop_plyrShouldntBeAbleToPostBlindsOutsidePreDeal :: Property
prop_plyrShouldntBeAbleToPostBlindsOutsidePreDeal = withDiscards 225 . property $ do
  g <- forAll $ genGame stages allPStates
  blind' <- forAll $ Gen.element [Small, Big]
  let action' = PostBlind blind'
      pName = "player1"
  isLeft (validateAction g pName action') === True
  where
    stages = [PreFlop, Flop, Turn, River, Showdown]

prop_plyrWithChipsShouldAlwaysBeableToFoldInTurn :: Property
prop_plyrWithChipsShouldAlwaysBeableToFoldInTurn = withDiscards 225 . property $ do
  g <- forAll $ genGame actionStages [In]
  let action' = Fold
      pName = "player0"
      inTurn = isRight $ isPlayerActingOutOfTurn g pName
      allIn = (== 0) $ _chips $ head $ _players g
      canFold = isRight $ validateAction g pName action'
  canFold === (inTurn && not allIn)
  where
    actionStages = [PreFlop, Flop, Turn, River]

spec = do
  describe "Player Acting in Turn Validation" $ do
    let game =
          (currentPosToAct ?~ 0)
            . (street .~ PreFlop)
            . (players .~ playerFixtures)
            $ initialGameState'
    it
      "returns Just OutOfTurn Error if given player is not in current position to act"
      $ do
        let playerName = "player3"
        let expectedErr =
              Left $
                InvalidMove playerName $ OutOfTurn $ CurrentPlayerToActErr "player1"
        isPlayerActingOutOfTurn game playerName `shouldBe` expectedErr

    it
      "returns return Right () when player is acting in turn during heads up game"
      $ do
        let game =
              (street .~ PreFlop)
                . (dealer .~ 0)
                . (currentPosToAct ?~ 1)
                . ( players
                      .~ [ ( (playerState .~ In)
                               . (actedThisTurn .~ False)
                               . (bet .~ 25)
                               . (committed .~ 25)
                           )
                             player1,
                           ( (playerState .~ In)
                               . (actedThisTurn .~ False)
                               . (bet .~ 50)
                               . (committed .~ 50)
                           )
                             player2
                         ]
                  )
                $ initialGameState'
        let playerName2 = "player2"
        isPlayerActingOutOfTurn game playerName2 `shouldBe` Right ()
        let game2 =
              (street .~ PreFlop)
                . (dealer .~ 1)
                . (currentPosToAct ?~ 0)
                . ( players
                      .~ [ ( (playerState .~ In)
                               . (actedThisTurn .~ False)
                               . (bet .~ 50)
                               . (committed .~ 50)
                           )
                             player1,
                           ( (playerState .~ In)
                               . (actedThisTurn .~ True)
                               . (bet .~ 50)
                               . (committed .~ 50)
                           )
                             player2
                         ]
                  )
                $ initialGameState'
        let playerName1 = "player1"
        isPlayerActingOutOfTurn game2 playerName1 `shouldBe` Right ()

    it "return no Error if player is acting in turn" $
      isPlayerActingOutOfTurn game "player1" `shouldBe` Right ()

    it "return no error for player acting in turn calling an all in during 2 plyr game" $
      isPlayerActingOutOfTurn callAllInHeadsUpFixture "player0" `shouldBe` Right ()

    it
      "returns Just NotAtTable Error if no player with playerName is sat at table"
      $ do
        let expectedErr = Left $ NotAtTable "MissingPlayer"
        checkPlayerSatAtTable game "MissingPlayer" `shouldBe` expectedErr

  describe "canBet" $ do
    it
      "should return NotEnoughChipsForAction InvalidMoveErr if raise value is greater than remaining chips"
      $ do
        let game2 =
              (players .~ playerFixtures2) . (street .~ Flop) $ initialGameState'
            playerName = "player3"
            amount = 10000
            expectedErr = Left $ InvalidMove playerName $ NotEnoughChipsForAction
        canBet playerName amount game2 `shouldBe` expectedErr

    it
      "should return CannotBetShouldRaiseInstead InvalidMoveErr if players have already bet or raised already"
      $ do
        let game2 =
              (players .~ playerFixtures) . (street .~ Flop) . (maxBet .~ 100) $
                initialGameState'
        let playerName = "player3"
        let amount = 50
        let errMsg =
              "A bet can only be carried out if no preceding player has bet"
        let expectedErr =
              Left $ InvalidMove playerName $ CannotBetShouldRaiseInstead errMsg
        canBet playerName amount game2 `shouldBe` expectedErr

    it
      "should return BetLessThanBigBlind InvalidMoveErr if bet is less than the current big blind"
      $ do
        let game2 =
              (players .~ playerFixtures2) . (street .~ Flop) $ initialGameState'
        let playerName = "player3"
        let amount = 2
        let expectedErr = Left $ InvalidMove playerName $ BetLessThanBigBlind
        canBet playerName amount game2 `shouldBe` expectedErr

    it "should not return an error if player can bet" $ do
      let game2 =
            (players .~ playerFixtures2) . (maxBet .~ 0) . (street .~ Flop) $
              initialGameState'
      let playerName = "player3"
      let amount = 100
      canBet playerName amount game2 `shouldBe` Right ()

    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal"
      $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let amount = 100
        let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
        canBet playerName amount preDealGame `shouldBe` expectedErr

    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown"
      $ do
        let showdownGame =
              (street .~ Showdown) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let amount = 100
        let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
        canBet playerName amount showdownGame `shouldBe` expectedErr

  describe "canRaise" $ do
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal"
      $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let amount = 100
        let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
        canBet playerName amount preDealGame `shouldBe` expectedErr

    it "should return InvalidActionForStreet if game stage is PreDeal" $ do
      let game =
            (street .~ PreDeal) . (players .~ playerFixtures) $
              initialGameState'
      let playerName = "player3"
      let amount = 50
      let minRaise = 400
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canRaise playerName amount game `shouldBe` expectedErr

    it
      "should be able to raise all in when chip count is less than minimum raise amount"
      $ do
        let game =
              (street .~ PreFlop) . (players .~ playerFixtures) . (maxBet .~ 200) $
                initialGameState'
        let playerName = "player3"
        let amount = 300
        canRaise playerName amount game `shouldBe` Right ()

    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal"
      $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let amount = 100
        let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
        canRaise playerName amount preDealGame `shouldBe` expectedErr

    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown"
      $ do
        let showdownGame =
              (street .~ Showdown) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let amount = 100
        let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
        canRaise playerName amount showdownGame `shouldBe` expectedErr

  describe "canCheck" $ do
    it
      "should return CannotCheckShouldCallRaiseOrFold InvalidMoveErr if maxBet is greater than zero and player bet is not equal to maxBet"
      $ do
        let game =
              (street .~ PreFlop) . (players .~ playerFixtures) . (maxBet .~ 200) $
                initialGameState'
        let playerName = "player3"
        let expectedErr =
              Left $ InvalidMove playerName $ CannotCheckShouldCallRaiseOrFold
        canCheck playerName game `shouldBe` expectedErr

    it
      "should allow Big Blind player to check during PreFlop when no bets or raises have occurred"
      $ do
        let game =
              (street .~ PreFlop)
                . ( players
                      .~ [ ( (playerState .~ In)
                               . (actedThisTurn .~ True)
                               . (bet .~ 50)
                               . (committed .~ 50)
                           )
                             player1,
                           ( (playerState .~ In)
                               . (actedThisTurn .~ False)
                               . (bet .~ 50)
                               . (committed .~ 50)
                           )
                             player2
                         ]
                  )
                $ initialGameState'
        let playerName = "player2"
        canCheck playerName game `shouldBe` Right ()

    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal"
      $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let amount = 100
        let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
        canCheck playerName preDealGame `shouldBe` expectedErr

    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown"
      $ do
        let showdownGame =
              (street .~ Showdown) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let amount = 100
        let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
        canCheck playerName showdownGame `shouldBe` expectedErr

    it "should be able to check when have chips and in position during 3 player game" $ do
      let playerName = "player0"
      validateAction turnGameThreePlyrs playerName Check `shouldBe` Right ()

  describe "canCall" $ do
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal"
      $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let amount = 100
        let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
        canCall playerName preDealGame `shouldBe` expectedErr

    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown"
      $ do
        let showdownGame =
              (street .~ Showdown) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let amount = 100
        let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
        canCall playerName showdownGame `shouldBe` expectedErr

    it
      "should return CannotCallZeroAmountCheckOrBetInstead InvalidMoveErr if game stage is not Preflop"
      $ do
        let game =
              (street .~ Flop) . (maxBet .~ 0) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player5"
        let expectedErr =
              Left $
                InvalidMove playerName $ CannotCallZeroAmountCheckOrBetInstead
        canCall playerName game `shouldBe` expectedErr

    it "should not return error if call bigBlind during Preflop" $ do
      let game =
            (street .~ PreFlop) . (players .~ playerFixtures2) $
              initialGameState'
      let playerName = "player5"
      canCall playerName game `shouldBe` Left (InvalidMove "player5" CannotCallZeroAmountCheckOrBetInstead)

  describe "canFold" $ do
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal"
      $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
        canFold playerName preDealGame `shouldBe` expectedErr

    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown"
      $ do
        let showdownGame =
              (street .~ Showdown) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
        canFold playerName showdownGame `shouldBe` expectedErr

  describe "canShowOrMuckHand" $ do
    it "should return InvalidMoveErr if game stage is not Showdown" $ do
      let preDealGame =
            (street .~ PreDeal) . (players .~ playerFixtures2) $
              initialGameState'
      let playerName = "player3"
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canShowOrMuckHand playerName preDealGame `shouldBe` expectedErr

    it "should return InvalidMoveErr if hand is not a singlePlayer showdown" $ do
      let showdownGame =
            (street .~ Showdown)
              . (pot .~ 1000)
              . (deck .~ initialDeck)
              . ( winners
                    .~ MultiPlayerShowdown [((Pair, PlayerShowdownHand []), "player4")]
                )
              . ( players
                    .~ [ ((playerState .~ In) . (actedThisTurn .~ True)) player4,
                         ((playerState .~ In) . (actedThisTurn .~ True)) player5
                       ]
                )
              $ initialGameState'
      let playerName = "player5"
      let expectedErr =
            Left $
              InvalidMove playerName $
                CannotShowHandOrMuckHand
                  "Can only show or muck cards if winner of single player pot during showdown"
      canShowOrMuckHand playerName showdownGame `shouldBe` expectedErr

    it
      "should return InvalidMoveErr if action was not sent by winner of single player showdown"
      $ do
        let showdownGame =
              (street .~ Showdown)
                . (pot .~ 1000)
                . (deck .~ initialDeck)
                . (winners .~ SinglePlayerShowdown "player4")
                . ( players
                      .~ [ ((playerState .~ In) . (actedThisTurn .~ True)) player4,
                           ((playerState .~ Folded) . (actedThisTurn .~ True)) player5
                         ]
                  )
                $ initialGameState'
        let playerName = "player5"
        let expectedErr =
              Left $
                InvalidMove playerName $
                  CannotShowHandOrMuckHand "Not winner of hand"
        canShowOrMuckHand playerName showdownGame `shouldBe` expectedErr

    it
      "should return no InvalidMoveErr if action was sent by winner of single player showdown"
      $ do
        let showdownGame =
              (street .~ Showdown)
                . (pot .~ 1000)
                . (deck .~ initialDeck)
                . (winners .~ SinglePlayerShowdown "player4")
                . ( players
                      .~ [ ((playerState .~ In) . (actedThisTurn .~ True)) player4,
                           ((playerState .~ Folded) . (actedThisTurn .~ True)) player5
                         ]
                  )
                $ initialGameState'
        let playerName = "player4"
        canShowOrMuckHand playerName showdownGame `shouldBe` Right ()

  describe "canTimeout" $ do
    it
      "should return an error for Timeout if no player can act"
      $ do
        let preFlopGame =
              (street .~ PreFlop) . (players .~ playerFixtures2) $
                initialGameState'
        let playerName = "player3"
        let expectedErr =
              Left $
                InvalidMove "player3" NoPlayerCanAct
        validateAction preFlopGame playerName Timeout `shouldBe` expectedErr

    it "should return no error for Timeout when acting in turn" $ do
      let preFlopGame = initialGameState' & (street .~ PreFlop) . (players .~ playerFixtures2) . (currentPosToAct ?~ 1)
      let playerName = "player5"
      validateAction preFlopGame playerName Timeout `shouldBe` Right ()

    it
      "should return InvalidActionForStreet InvalidMoveErr if Timeout action occurs during Showdown"
      $ do
        let showDownGame = initialGameState' & (street .~ Showdown) . (players .~ playerFixtures2)
        let playerName = "player3"
        let expectedErr = Left $ InvalidMove playerName InvalidActionForStreet
        validateAction showDownGame playerName Timeout `shouldBe` expectedErr

    it "should return err for LeaveSeat if game state is not PreDeal" $ do
      let preFlopGame = initialGameState' & (street .~ PreFlop) . (players .~ playerFixtures2)
      let playerName = "player3"
      let expectedErr = InvalidMove "player3" CannotLeaveSeatOutsidePreDeal
      validateAction preFlopGame playerName LeaveSeat'
        `shouldBe` Left expectedErr

    it "should return err for LeaveSeat if player is not sat at Table" $ do
      let preDealGame = initialGameState' & (street .~ PreDeal) . (players .~ playerFixtures2)
      let playerName = "playerX"
      let expectedErr = NotAtTable playerName
      validateAction preDealGame playerName LeaveSeat'
        `shouldBe` Left expectedErr

    it
      "should return no err for leave seat if player is sat at table during PreDeal"
      $ do
        let preDealGame = initialGameState' & (street .~ PreDeal) . (players .~ playerFixtures2)
        let playerName = "player3"
        validateAction preDealGame playerName LeaveSeat' `shouldBe` Right ()

  describe "canSitOut" $ do
    it
      "should allow player to sit out of the game during the PreDeal street if sat in"
      $ do
        let preDealGame =
              (street .~ PreDeal)
                . ( players
                      .~ [ ( (playerState .~ In)
                               . (actedThisTurn .~ False)
                               . (bet .~ 0)
                               . (committed .~ 0)
                           )
                             player1,
                           ( (playerState .~ In)
                               . (actedThisTurn .~ False)
                               . (bet .~ 0)
                               . (committed .~ 0)
                           )
                             player2
                         ]
                  )
                $ initialGameState'
        let playerName = "player1"
        validateAction preDealGame playerName SitOut `shouldBe` Right ()

    it "should return error if player is not at table" $ do
      let preDealGame =
            (street .~ PreFlop)
              . ( players
                    .~ [ ( (playerState .~ In)
                             . (actedThisTurn .~ True)
                             . (bet .~ 50)
                             . (committed .~ 50)
                         )
                           player1,
                         ( (playerState .~ In)
                             . (actedThisTurn .~ False)
                             . (bet .~ 50)
                             . (committed .~ 50)
                         )
                           player2
                       ]
                )
              $ initialGameState'
      let playerName = "player3"
      let expectedErr = Left $ NotAtTable playerName
      validateAction preDealGame playerName SitOut `shouldBe` expectedErr

    it "should not allow player to sit out of the game if already sat out" $ do
      let preDealGame =
            (street .~ PreDeal)
              . ( players
                    .~ [ ( (playerState .~ SatOut)
                             . (actedThisTurn .~ False)
                             . (bet .~ 0)
                             . (committed .~ 0)
                         )
                           player1,
                         ( (playerState .~ In)
                             . (actedThisTurn .~ False)
                             . (bet .~ 0)
                             . (committed .~ 0)
                         )
                           player2
                       ]
                )
              $ initialGameState'
      let playerName = "player1"
      let expectedErr = Left $ InvalidMove playerName AlreadySatOut
      validateAction preDealGame playerName SitOut `shouldBe` expectedErr

    it "should not allow player to sit out unless street is PreDeal" $ do
      let preDealGame =
            (street .~ PreFlop)
              . ( players
                    .~ [ ( (playerState .~ In)
                             . (actedThisTurn .~ True)
                             . (bet .~ 50)
                             . (committed .~ 50)
                         )
                           player1,
                         ( (playerState .~ In)
                             . (actedThisTurn .~ False)
                             . (bet .~ 50)
                             . (committed .~ 50)
                         )
                           player2
                       ]
                )
              $ initialGameState'
      let playerName = "player2"
      let expectedErr = Left $ InvalidMove playerName CannotSitOutOutsidePreDeal
      validateAction preDealGame playerName SitOut `shouldBe` expectedErr

  describe "validateBlindAction" $
    describe "Heads Up Game" $ do
      let game' =
            (street .~ PreDeal)
              . (maxBet .~ 0)
              . (pot .~ 0)
              . (deck .~ initialDeck)
              . (currentPosToAct ?~ 1)
              . (dealer .~ 0)
              . ( players
                    .~ [ ( (actedThisTurn .~ False)
                             . (playerState .~ In)
                             . (bet .~ 0)
                             . (chips .~ 2000)
                             . (committed .~ 0)
                         )
                           player1,
                         ( (actedThisTurn .~ False)
                             . (playerState .~ In)
                             . (bet .~ 0)
                             . (committed .~ 0)
                             . (chips .~ 2000)
                         )
                           player2
                       ]
                )
              $ initialGameState'

      it "Player1 should require small blind" $
        validateBlindAction game' (_playerName player1) Small
          `shouldBe` Right ()

      it "Player2 should require bigBlind" $
        validateBlindAction game' (_playerName player2) Big `shouldBe` Right ()

  describe "canPostBlind" $
    describe "Heads Up Game" $ do
      let game' =
            (street .~ PreDeal)
              . (maxBet .~ 0)
              . (pot .~ 0)
              . (deck .~ initialDeck)
              . (currentPosToAct ?~ 1)
              . (dealer .~ 0)
              . ( players
                    .~ [ ( (actedThisTurn .~ False)
                             . (playerState .~ In)
                             . (bet .~ 0)
                             . (chips .~ 2000)
                             . (committed .~ 0)
                         )
                           player1,
                         ( (actedThisTurn .~ False)
                             . (playerState .~ In)
                             . (bet .~ 0)
                             . (committed .~ 0)
                             . (chips .~ 2000)
                         )
                           player2
                       ]
                )
              $ initialGameState'
      it "Player1 should be able to post small blind" $
        canPostBlind game' (_playerName player1) Small `shouldBe` Right ()

  describe "validateAction" $ do
    describe "postBlinds" $ do
      it "Player0 should be able to post small blind" $ do
        let action' = PostBlind Small
        let pName = "player0"
        validateAction preDealHeadsUpFixture pName action' `shouldBe` Right ()

      it "Player0 should not be able post big blind" $ do
        let action' = PostBlind Big
        let pName = "player0"
        isLeft (validateAction preDealHeadsUpFixture pName action') `shouldBe` True

      it "Player1 should not be able post big blind when already posted big blind" $ do
        let action' = PostBlind Big
        let pName = "player1"
        isLeft (validateAction preDealHeadsUpFixture pName action') `shouldBe` True

      it "Player1 should not be able post small blind when already posted big blind" $ do
        let action' = PostBlind Small
        let pName = "player1"
        isLeft (validateAction preDealHeadsUpFixture pName action') `shouldBe` True

      it "Players can't post a blind when they have no chips" $ require prop_plyrShouldntBeAbleToPostBlindsWhenNoChips

      it "Players shouldn't be able to post blinds outside PreDeal" $ require prop_plyrShouldntBeAbleToPostBlindsOutsidePreDeal

    describe "fold" $
      it "should always be able to fold when in turn" $ require prop_plyrWithChipsShouldAlwaysBeableToFoldInTurn