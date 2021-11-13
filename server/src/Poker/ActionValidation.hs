--  TODO - should factor out the hasEnoughChips check for each action and then just sequence it
--  inside the parent validateAction function with >>
--
-- Second TODo - remove use of fromJust
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.ActionValidation where

import Control.Monad (when)
import Control.Monad.State.Lazy (when)
import Data.List (elemIndex, find)
import qualified Data.List.Safe as Safe
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Poker.Game.Blinds (blindRequiredByPlayer)
import Poker.Game.Game
  ( doesPlayerHaveToAct,
    getWinners,
  )
import Poker.Game.Utils
  ( getActivePlayers,
    getGamePlayer,
    getGamePlayerNames,
    getGamePlayerState,
    getPlayerNames,
  )
import Poker.Types

-- TODO -- We should be deeply suspicious of the type  m ()
-- as we are losing info and it is easy to forget to validate actions.
-- Instead the validation should be done behind an abstract data type
-- (think smart consstructyor) such as
-- mkBet :: Game -> Amount -> PlayerInfo -> Either BetErr Bet
--
-- Another benefit is that instead of a monolothic game error we
-- now hae BetErr FoldErr which is much nicer.
validateAction :: Game -> PlayerName -> Action -> Either GameErr ()
validateAction game@Game {..} name' = \case
  PostBlind blind ->
    when (_maxBet > 0) (isPlayerActingOutOfTurn game name')
      >> checkPlayerSatAtTable game name'
      >> canPostBlind game name' blind
      >> validateBlindAction game name' blind
  Check -> isPlayerActingOutOfTurn game name' >> canCheck name' game
  Fold -> isPlayerActingOutOfTurn game name' >> canFold name' game
  Bet amount -> isPlayerActingOutOfTurn game name' >> canBet name' amount game
  Raise amount ->
    isPlayerActingOutOfTurn game name' >> canRaise name' amount game
  Call -> isPlayerActingOutOfTurn game name' >> canCall name' game
  Timeout -> canTimeout name' game
  LeaveSeat' -> canLeaveSeat name' game
  SitDown plyr -> canSit plyr game
  SitOut -> checkPlayerSatAtTable game name' >> canSitOut name' game
  SitIn -> checkPlayerSatAtTable game name' >> canSitIn name' game
  ShowHand -> validateShowOrMuckHand game name' ShowHand
  MuckHand -> validateShowOrMuckHand game name' MuckHand

-- Cannot post a blind to start a game unless at least two active players are present.
-- An active player is one whose playerStatus is set to In.
canPostBlind :: Game -> PlayerName -> Blind -> Either GameErr ()
canPostBlind game@Game {..} name blind
  | _street /= PreDeal = Left $ InvalidMove name InvalidActionForStreet
  | activePlayersCount < 2 =
    Left $
      InvalidMove name $
        CannotPostBlind
          "Cannot post blind unless a minimum of two active players are sat at table"
  | otherwise = case blind of
    BigBlind -> if unChips chipCount < _bigBlind then notEnoughChipsErr else Right ()
    SmallBlind -> if unChips chipCount < _smallBlind then notEnoughChipsErr else Right ()
  where
    chipCount = _chips $ fromJust $ getGamePlayer game name
    activePlayersCount = length $ getActivePlayers _players
    notEnoughChipsErr = Left $ InvalidMove name NotEnoughChipsForAction

-- | The first player to post their blinds in the predeal stage can do it from any
-- position as long as there aren't enough players sat in to start a game
-- Therefore the acting in turn rule wont apply for that first move
-- when (< 2 players state set to sat in)
isPlayerActingOutOfTurn :: Game -> PlayerName -> Either GameErr ()
isPlayerActingOutOfTurn game@Game {..} name
  | isNewGame = Right () -- Only Permit First Blind Posting to Be at Any Position When
  -- starting new Game"
  | currPosToActOutOfBounds = error "_currentPosToAct too big"
  | isNothing _currentPosToAct && _street /= PreDeal = Left $ InvalidMove name $ NoPlayerCanAct
  | fromJust _currentPosToAct < 0 = error "_currentPosToAct player < 0"
  | otherwise = case name `elemIndex` gamePlayerNames of
    Nothing -> Left $ NotAtTable name
    Just pos ->
      if doesPlayerHaveToAct name game
        then Right ()
        else
          Left $
            InvalidMove name $
              OutOfTurn $
                CurrentPlayerToActErr $
                  gamePlayerNames
                    !! fromJust _currentPosToAct
  where
    gamePlayerNames = getGamePlayerNames game
    numberOfPlayersSatIn =
      length $ filter (\PlayerInfo {..} -> _playerStatus /= InHand Folded) _players
    currPosToActOutOfBounds =
      maybe False ((length _players - 1) <) _currentPosToAct
    isNewGame = _street == PreDeal && isNothing _currentPosToAct

checkPlayerSatAtTable :: Game -> PlayerName -> Either GameErr ()
checkPlayerSatAtTable game@Game {..} name
  | not atTable = Left $ NotAtTable name
  | otherwise = Right ()
  where
    playerNames = getGamePlayerNames game
    atTable = name `elem` playerNames

canTimeout :: PlayerName -> Game -> Either GameErr ()
canTimeout name game@Game {..}
  | _street == Showdown = Left $ InvalidMove name InvalidActionForStreet
  | otherwise = isPlayerActingOutOfTurn game name

canBet :: PlayerName -> Chips -> Game -> Either GameErr ()
canBet name amount game@Game {..}
  | unChips amount < _bigBlind =
    Left $ InvalidMove name BetLessThanBigBlind
  | amount > chipCount =
    Left $ InvalidMove name NotEnoughChipsForAction
  | _street == Showdown || _street == PreDeal =
    Left $ InvalidMove name InvalidActionForStreet
  | _maxBet > 0 && _street /= PreFlop =
    Left $
      InvalidMove name $
        CannotBetShouldRaiseInstead
          "A bet can only be carried out if no preceding player has bet"
  | otherwise =
    Right ()
  where
    chipCount = _chips $ fromJust $ getGamePlayer game name

-- Keep in mind that a player can always raise all in,
-- even if their total chip count is less than what
-- a min-bet or min-raise would be.
canRaise :: PlayerName -> Chips -> Game -> Either GameErr ()
canRaise name amount game@Game {..}
  | _street == Showdown || _street == PreDeal =
    Left $ InvalidMove name InvalidActionForStreet
  | _street == PreFlop && unChips _maxBet == _bigBlind =
    Left $ InvalidMove name CannotRaiseShouldBetInstead -- a blind doesnt count as a sufficient bet to qualify a raise
  | _maxBet == 0 =
    Left $ InvalidMove name CannotRaiseShouldBetInstead
  | amount < minRaise && amount /= chipCount =
    Left $ InvalidMove name $ RaiseAmountBelowMinRaise $ unChips minRaise
  | amount > chipCount =
    Left $ InvalidMove name NotEnoughChipsForAction
  | otherwise =
    Right ()
  where
    minRaise = 2 * _maxBet
    chipCount = _chips $ fromJust $ getGamePlayer game name

canCheck :: PlayerName -> Game -> Either GameErr ()
canCheck name Game {..}
  | _street == PreFlop && fromCommittedChips _committed < _bigBlind =
    Left $
      InvalidMove name CannotCheckShouldCallRaiseOrFold
  | _street == Showdown || _street == PreDeal =
    Left $
      InvalidMove name InvalidActionForStreet
  | fromCommittedChips _committed < unChips _maxBet =
    Left $
      InvalidMove name CannotCheckShouldCallRaiseOrFold
  | otherwise = Right ()
  where
    PlayerInfo {..} = fromJust $ find (\PlayerInfo {..} -> _playerName == name) _players

canFold :: PlayerName -> Game -> Either GameErr ()
canFold name Game {..}
  | _street == Showdown || _street == PreDeal =
    Left $
      InvalidMove name InvalidActionForStreet
  | otherwise = Right ()

canCall :: PlayerName -> Game -> Either GameErr ()
canCall name game@Game {..}
  | _street == Showdown || _street == PreDeal =
    Left $
      InvalidMove name InvalidActionForStreet
  | amountNeededToCall == 0 =
    Left $
      InvalidMove name CannotCallZeroAmountCheckOrBetInstead
  | otherwise = Right ()
  where
    p = fromJust (getGamePlayer game name)
    chipCount = _chips p
    amountNeededToCall = _maxBet - _bet p

canSit :: PlayerInfo -> Game -> Either GameErr ()
canSit player@PlayerInfo {..} game@Game {..}
  | _street /= PreDeal =
    Left $
      InvalidMove _playerName CannotSitDownOutsidePreDeal
  | _playerName `elem` getPlayerNames _players =
    Left $
      AlreadySatAtTable _playerName
  | _chips < _minBuyInChips = Left $ NotEnoughChips _playerName
  | _chips > _maxBuyInChips = Left $ OverMaxChipsBuyIn _playerName
  | length _players < _maxPlayers = Right ()
  | otherwise = Left $ CannotSitAtFullTable _playerName

canSitOut :: PlayerName -> Game -> Either GameErr ()
canSitOut name game@Game {..}
  | _street /= PreDeal = Left $ InvalidMove name CannotSitOutOutsidePreDeal
  | isNothing currentState = Left $ NotAtTable name
  | currentState == Just SatOut = Left $ InvalidMove name AlreadySatOut
  | otherwise = Right ()
  where
    currentState = getGamePlayerState game name

canSitIn :: PlayerName -> Game -> Either GameErr ()
canSitIn name game@Game {..}
  | _street /= PreDeal = Left $ InvalidMove name CannotSitInOutsidePreDeal
  | isNothing pState = Left $ NotAtTable name
  | maybe False satIn pState = Left $ InvalidMove name AlreadySatIn
  | otherwise = Right ()
  where
    pState = getGamePlayerState game name

canLeaveSeat :: PlayerName -> Game -> Either GameErr ()
canLeaveSeat playerName game@Game {..}
  | _street /= PreDeal =
    Left $
      InvalidMove playerName CannotLeaveSeatOutsidePreDeal
  | playerName `notElem` getPlayerNames _players = Left $ NotAtTable playerName
  | otherwise = Right ()

canJoinWaitList :: PlayerInfo -> Game -> Either GameErr ()
canJoinWaitList player@PlayerInfo {..} game@Game {..}
  | _playerName `elem` _waitlist = Left $ AlreadyOnWaitlist _playerName
  | otherwise = Right ()

validateBlindAction :: Game -> PlayerName -> Blind -> Either GameErr ()
validateBlindAction game@Game {..} playerName blind
  | _street /= PreDeal =
    Left $
      InvalidMove playerName CannotPostBlindOutsidePreDeal
  | otherwise = case getGamePlayer game playerName of
    Nothing -> Left $ PlayerNotAtTable playerName
    Just p@PlayerInfo {..} -> case blindRequired of
      Nothing -> Left $ InvalidMove playerName BlindNotRequired
      Just SmallBlind ->
        if blind == SmallBlind
          then
            if fromCommittedChips _committed >= _smallBlind
              then Left $ InvalidMove playerName $ BlindAlreadyPosted SmallBlind
              else Right ()
          else Left $ InvalidMove playerName $ BlindRequiredErr SmallBlind
      Just BigBlind ->
        if blind == BigBlind
          then
            if fromCommittedChips _committed >= bigBlindValue
              then Left $ InvalidMove playerName $ BlindAlreadyPosted BigBlind
              else Right ()
          else Left $ InvalidMove playerName $ BlindRequiredErr BigBlind
      where
        blindRequired = blindRequiredByPlayer game playerName
        bigBlindValue = _smallBlind * 2

validateShowOrMuckHand :: Game -> PlayerName -> Action -> Either GameErr ()
validateShowOrMuckHand game@Game {..} name action =
  checkPlayerSatAtTable game name

-- Should Tell us if everyone has folded to the given playerName
-- and the hand is over
canShowOrMuckHand :: PlayerName -> Game -> Either GameErr ()
canShowOrMuckHand name game@Game {..}
  | _street /= Showdown = Left $ InvalidMove name InvalidActionForStreet
  | otherwise = case _winners of
    SinglePlayerShowdown winningPlayerName ->
      if winningPlayerName == name
        then Right ()
        else
          Left $
            InvalidMove name $
              CannotShowHandOrMuckHand
                "Not winner of hand"
    MultiPlayerShowdown _ ->
      Left $
        InvalidMove name $
          CannotShowHandOrMuckHand
            "Can only show or muck cards if winner of single player pot during showdown"
