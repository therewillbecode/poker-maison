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
import Poker.Poker
import Control.Lens (element, (%~), (&), (.~), (?~))
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as T
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
    smallBlind,
    bigBlind,
    deck,
    maxBet,
    playerState,
    players,
    pot,
    street,
    winners,
  )
import Test.Hspec
import System.Random
import Test.Hspec.Hedgehog
  ( PropertyT,
    diff,
    forAll,
    hedgehog,
    modifyMaxDiscardRatio,
    (/==),
    (===),
  )
import Control.Monad
import Control.Monad.Reader


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

spec = describe "Poker" $ do

  let inactiveGame' =
          (street .~ PreDeal)
            . (smallBlind .~ 25)
            . (bigBlind .~ 50)
            . (maxBet .~ 50)
            . (pot .~ 50)
            . (deck .~ initialDeck)
            . (currentPosToAct ?~ 0)
            . (dealer .~ 0)
            . ( players
                  .~ [ ( (actedThisTurn .~ True) -- Should actedThisTurn really be used for predeal?
                           . (playerState .~ In)
                           . (bet .~ 50)
                           . (chips .~ 1450)
                           . (committed .~ 50)
                       )
                         player1,
                       ( (actedThisTurn .~ False)
                           . (playerState .~ In)
                           . (bet .~ 0)
                           . (committed .~ 0)
                           . (chips .~ 1975)
                       )
                         player2
                     ]
              )
            $ initialGameState'
  
  let expectedNewHand = 
          (street .~ PreDeal)
            . (smallBlind .~ 25)
            . (bigBlind .~ 50)
            . (maxBet .~ 0)
            . (pot .~ 0)
            . (deck .~ initialDeck)
            . (currentPosToAct .~ Nothing)
            . (dealer .~ 0)
            . ( players
                  .~ [ ( (actedThisTurn .~ False) -- Should actedThisTurn really be used for predeal?
                           . (playerState .~ In)
                           . (bet .~ 0)
                           . (chips .~ 1450)
                           . (committed .~ 50)
                       )
                         player1,
                       ( (actedThisTurn .~ False)
                           . (playerState .~ In)
                           . (bet .~ 0)
                           . (committed .~ 0)
                           . (chips .~ 1975)
                       )
                         player2
                     ]
              )
            $ inactiveGame'
  
  it "Game should progress to a new hand" $ do
      gen <- liftIO getStdGen
      progressGame gen inactiveGame' `shouldBe` expectedNewHand 
