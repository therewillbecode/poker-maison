{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game.Game where

import Control.Lens (Field2 (_2), (%~), (&), (.~), (?~), (^.))
import Data.List (find, mapAccumR)
import qualified Data.List.Safe as Safe
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import Poker.Game.Blinds
  ( blindRequiredByPlayer,
    haveRequiredBlindsBeenPosted,
    updatePlayersInHand,
  )
import Poker.Game.Hands (value)
import Poker.Game.Utils
  ( countActive,
    filterPlayersWithLtChips,
    filterSatOutPlayers,
    getActivePlayers,
    getPlayersSatIn,
    inPositionToAct,
    maximums,
    modDec,
    modInc,
  )
import Poker.Types
  ( Blind (NoBlind),
    Card,
    Deck (..),
    Game (..),
    HandRank,
    Player (..),
    PlayerName,
    PlayerShowdownHand (..),
    PlayerState (Folded, In, SatOut),
    PocketCards (..),
    Street (..),
    Winners (..),
    actedThisTurn,
    bet,
    currentPosToAct,
    maxBet,
    playerName,
    playerState,
    players,
    pockets,
    street,
    unDeck,
    unPocketCards,
    winners,
  )

-- | Returns both the dealt players and remaining cards left in deck.
-- We return the new deck for the purposes of dealing the board cards
-- over the remaining course of the hand.
dealToPlayers :: Deck -> [Player] -> (Deck, [Player])
dealToPlayers =
  mapAccumR
    ( \deck player ->
        if player ^. playerState == In
          then
            let (pocketCs, remainingDeck) = dealPockets deck
             in (remainingDeck, (pockets ?~ pocketCs) player)
          else (deck, player)
    )

dealPockets :: Deck -> (PocketCards, Deck)
dealPockets (Deck cs) = (PocketCards fstC sndC, Deck remainingDeck)
  where
    ([fstC, sndC], remainingDeck) = splitAt 2 cs

dealBoardCards :: Int -> Game -> Game
dealBoardCards n game@Game {..} =
  Game
    { _board = _board <> boardCards,
      _deck = Deck shuffledDeck,
      ..
    }
  where
    (boardCards, shuffledDeck) = splitAt n (unDeck _deck)

deal :: Game -> Game
deal game@Game {..} =
  Game
    { _players = dealtPlayers,
      _deck = remainingDeck,
      ..
    }
  where
    (remainingDeck, dealtPlayers) = dealToPlayers _deck _players

-- Gets the next data constructor of the Street which represents
-- the name of the next game stage.
-- The term Street is poker terminology for hand stage.
getNextStreet :: Street -> Street
getNextStreet Showdown = minBound
getNextStreet _street = succ _street

-- Unless in the scenario where everyone is all in
-- if no further player actions are possible (i.e betting has finished)
-- then actedThisTurn should be set to True for all active players in Hand.
-- This scenario occurs when all players or all but one players are all in.
resetPlayers :: Game -> Game
resetPlayers game@Game {..} = (players .~ newPlayers) game
  where
    newPlayers = (bet .~ 0) . (actedThisTurn .~ False) <$> _players

-- When there are only two players in game (Heads Up) then the first player
-- to act PreFlop is the player at the dealer position.
-- First player to act is the player sitting to the right of the player
-- in the big blind position
progressToPreFlop :: Game -> Game
progressToPreFlop game@Game {..} =
  game
    & (street .~ PreFlop)
      . (currentPosToAct .~ firstPosToAct)
      . (players %~ (<$>) (actedThisTurn .~ False))
      . deal
      . updatePlayersInHand
  where
    firstPosToAct
      | countActive _players == 2 = pure _dealer
      | -- When heads up dealer goes first
        otherwise =
        nextPosToAct game

progressToFlop :: Game -> Game
progressToFlop game
  | allButOneAllIn game =
    game & (street .~ Flop) . dealBoardCards 3
  | otherwise =
    game
      & (street .~ Flop)
        . (currentPosToAct .~ nextPosToAct game)
        . (maxBet .~ 0)
        . dealBoardCards 3
        . resetPlayers

progressToTurn :: Game -> Game
progressToTurn game
  | allButOneAllIn game =
    game & (street .~ Turn) . dealBoardCards 1
  | otherwise =
    game
      & (street .~ Turn)
        . (maxBet .~ 0)
        . (currentPosToAct .~ nextPosToAct game)
        . dealBoardCards 1
        . resetPlayers

progressToRiver :: Game -> Game
progressToRiver game@Game {..}
  | allButOneAllIn game =
    game & (street .~ River) . dealBoardCards 1
  | otherwise =
    game
      & (street .~ River)
        . (maxBet .~ 0)
        . (currentPosToAct .~ nextPosToAct game)
        . dealBoardCards 1
        . resetPlayers

progressToShowdown :: Game -> Game
progressToShowdown game@Game {..} =
  game
    & (street .~ Showdown)
      . (winners .~ winners')
      . (players .~ awardedPlayers)
      . (currentPosToAct .~ Nothing)
  where
    winners' = getWinners game
    awardedPlayers = awardWinners _players _pot winners'

-- need to give players the chips they are due and split pot if necessary
-- if only one active player then this is a result of everyone else folding
-- and they are awarded the entire pot
--
-- If only one player is active during the showdown stage then this means all other players
-- folded to him. The winning player then has the choice of whether to "muck"
-- (not show) his cards or not.
-- SinglePlayerShowdown occurs when everyone folds to one player
awardWinners :: [Player] -> Int -> Winners -> [Player]
awardWinners _players pot' = \case
  MultiPlayerShowdown winners' ->
    let chipsPerPlayer = pot' `div` length winners'
        playerNames = snd <$> winners'
     in ( \p@Player {..} ->
            if _playerName `elem` playerNames
              then Player {_chips = _chips + chipsPerPlayer, ..}
              else p
        )
          <$> _players
  SinglePlayerShowdown _ ->
    ( \p@Player {..} ->
        if p `elem` getActivePlayers _players
          then Player {_chips = _chips + pot', ..}
          else p
    )
      <$> _players

-- Can we show the active players' pocket cards to the world? Only if everyone is all in
-- (no more than 1 player not all (> 0 chips) per pot and every player has acted
canPubliciseActivesCards :: Game -> Bool
canPubliciseActivesCards g =
  haveAllPlayersActed g && allButOneAllIn g
    || everyoneAllIn g
    || multiplayerShowdown
  where
    multiplayerShowdown =
      _street g == Showdown && isMultiPlayerShowdown (_winners g)

-- TODO move players from waitlist to players list
-- TODO need to send msg to players on waitlist when a seat frees up to inform them
-- to choose a seat and set limit for them t pick one
-- TODO - have newBlindNeeded field which new players will initially be put into in order to
-- ensure they cant play without posting a blind before the blind position comes round to them
-- new players can of course post their blinds early. In the case of an early posting the initial
-- blind must be the big blind. After this 'early' blind or the posting of a normal blind in turn the
-- new player will be removed from the newBlindNeeded field and can play normally.
getNextHand :: Game -> Deck -> Game
getNextHand Game {..} shuffledDeck =
  Game
    { _waitlist = newWaitlist,
      _maxBet = 0,
      _players = newPlayers,
      _board = [],
      _deck = shuffledDeck,
      _winners = NoWinners,
      _street = PreDeal,
      _dealer = newDealer,
      _pot = 0,
      _currentPosToAct = Just nextPlayerToAct,
      ..
    }
  where
    incAmount = 1
    newDealer = modInc incAmount _dealer (length (getPlayersSatIn _players) - 1)
    freeSeatsNo = _maxPlayers - length _players
    newPlayers =
      filterSatOutPlayers $
        filterPlayersWithLtChips _bigBlind $
          resetPlayerCardsAndBets
            <$> _players
    newWaitlist = drop freeSeatsNo _waitlist
    nextPlayerToAct = modInc incAmount newDealer (length newPlayers - 1)

-- | If all players have acted and their bets are equal
-- to the max bet then we can move to the next stage
haveAllPlayersActed :: Game -> Bool
haveAllPlayersActed g@Game {..}
  | _street == Showdown = True
  | _street == PreDeal = haveRequiredBlindsBeenPosted g
  | length activePlayers < 2 = True
  | otherwise = not (awaitingPlayerAction g)
  where
    activePlayers = getActivePlayers _players

awaitingPlayerAction :: Game -> Bool
awaitingPlayerAction Game {..} =
  length activePlayers >= 2 && any (canAct _maxBet) activePlayers
  where
    activePlayers = getActivePlayers _players
    canAct maxBet' Player {..} =
      _playerState /= Folded
        && _chips > 0
        && (not _actedThisTurn || _actedThisTurn && (_bet < maxBet'))

-- If all players have folded apart from a remaining player then the mucked boolean
-- inside the player value will determine if we show the remaining players hand to the
-- table.
--
-- Otherwise we just get the handrankings of all active players.
getWinners :: Game -> Winners
getWinners game@Game {..} =
  if allButOneFolded game
    then
      SinglePlayerShowdown $
        head $
          flip (^.) playerName
            <$> filter (\Player {..} -> _playerState == In) _players
    else MultiPlayerShowdown $ maximums $ getHandRankings _players _board

-- Return the best hands and the active players (playerState of In) who hold
-- those hands.
--
-- If more than one player holds the same winning hand then the second part
-- of the tuple will consist of all the players holding the hand
getHandRankings ::
  [Player] -> [Card] -> [((HandRank, PlayerShowdownHand), PlayerName)]
getHandRankings plyrs boardCards =
  ( \(showdownHand, Player {..}) ->
      ((_2 %~ PlayerShowdownHand) showdownHand, _playerName)
  )
    <$> map
      ( \plyr@Player {..} ->
          let showHand = (++ boardCards) $ unPocketCards $ fromJust _pockets
           in (value showHand, plyr)
      )
      remainingPlayersInHand
  where
    remainingPlayersInHand = getActivePlayers plyrs

-- Update active players states to prepare them for the next hand.
resetPlayerCardsAndBets :: Player -> Player
resetPlayerCardsAndBets Player {..} =
  Player
    { _pockets = Nothing,
      _playerState = newPlayerState,
      _bet = 0,
      _committed = 0,
      _actedThisTurn = False,
      ..
    }
  where
    newPlayerState
      | _chips == 0 = SatOut
      | _playerState == Folded || _playerState == In = In
      | otherwise = SatOut

-- The game should go straight to showdown if all but one players is In hand
allButOneFolded :: Game -> Bool
allButOneFolded game@Game {..} =
  _street /= PreDeal && length playersInHand <= 1
  where
    playersInHand = filter ((== In) . (^. playerState)) _players

initPlayer :: Text -> Int -> Player
initPlayer playerName chips =
  Player
    { _pockets = Nothing,
      _bet = 0,
      _playerState = In,
      _playerName = playerName,
      _possibleActions = [],
      _committed = 0,
      _actedThisTurn = False,
      _chips = chips
    }

-- During PreDeal we start timing out players who do not post their respective blinds
-- in turn after an initial blind has been posted
--
-- No player is forced to post first blind during PreDeal (blind betting stage).
--
-- Important to note that this function is mainly for asserting whether we need to
-- time a player's action. Player actions which are not mandatory such as posting a blind
-- to start a game will not be timed actions.
--
-- All possible player actions are either compulsary or optional. For example SitIn as a player is never forced to play a game. However if a player is already active in an
-- ongoing hand then all future actions for this hand will be mandatory and therefore timed so that a given
-- player cannot postpone the game through inactivity for an indefinite amount of time.
--
-- Optional actions as as SitIn (changing player state to In to denote that they are active this hand)
-- would return False in this function.
--
-- PostBlind actions are trickier. Depending on the context they will be compulsary or optional.
-- True is for a situation where the continued progression
-- of the game in a satisfactory timeframe is determined by the expediancy of the current
-- player's action.
doesPlayerHaveToAct :: Text -> Game -> Bool
doesPlayerHaveToAct playerName game@Game {..}
  | length _players < 2 = False
  | not $ inPositionToAct playerName game = False
  | isNothing _currentPosToAct = False
  | otherwise =
    if currPosToActOutOfBounds
      then error $ "_currentPosToAct too large " <> show game
      else case _players Safe.!! fromJust _currentPosToAct of
        Nothing -> False
        Just Player {..}
          | _chips == 0 ->
            False
          | _street
              == Showdown
              || activePlayerCount < 2
              || haveAllPlayersActed game
              || _playerState
              /= In
              || _street
              == PreDeal && _maxBet
              == 0 ->
            False
          | _street == PreDeal ->
            _playerName
              == playerName
              && blindRequiredByPlayer game playerName /= NoBlind
          | otherwise ->
            _playerName == playerName
  where
    activePlayerCount =
      length $ filter (\Player {..} -> _playerState == In) _players
    currPosToActOutOfBounds =
      maybe False ((length _players - 1) <) _currentPosToAct

-- returns the index of the next active player with chips after the given current index who has chips
--
-- return a Nothing if everyone is all in and we cannot increment the position since
-- there are no players who can act

-- updates position to next player if there is another player who can act otherwise
-- return the same position

-- second param is the position we start from when calculating the next position to act
-- unless this is the beginning of the next stage then the initialPos == _currentPosToAct
nextIPlayerToAct :: Game -> Maybe Int -> Maybe (Int, Player)
nextIPlayerToAct Game {..} = nextIPlayer
  where
    iplayers = zip [0 ..] _players
    iplayers' currPosToAct =
      let (a, b) = splitAt currPosToAct iplayers in b <> a
    nextIPlayer = \case
      Nothing -> Nothing
      Just currPos ->
        let nextIPlayerToAct = tail $ iplayers' currPos
         in find
              (\(_, Player {..}) -> _playerState == In && _chips > 0)
              nextIPlayerToAct

-- gets the position of the next player which needs to act
-- if currentPosToAct is already a Nothing then this means we are starting a new hand
-- and will just return the initial player to act for a new hand
nextPosToAct :: Game -> Maybe Int
nextPosToAct g@Game {..}
  | haveAllPlayersActed g && not (everyoneAllIn g) = firstPosToAct g
  | everyoneAllIn g = Nothing
  | otherwise = fst <$> nextIPlayerToAct g _currentPosToAct
  where
    activePs = getActivePlayers _players
    activePCount = length activePs

-- used to get the initial player to act when progressing to a new game stage
firstPosToAct :: Game -> Maybe Int
firstPosToAct g@Game {..}
  | activePCount == 2 =
    if _street == PreFlop
      then firstPToAct _dealer
      else firstPToAct 1
  | otherwise = if _street == PreFlop then firstPToAct 3 else firstPToAct 1
  where
    activePs = getActivePlayers _players
    activePCount = length activePs
    incPosBy n = Just $ modInc n _dealer (activePCount - 1)
    firstPToAct dealerIncAmount =
      fst <$> nextIPlayerToAct g (incPosBy (modDec dealerIncAmount activePCount))

isMultiPlayerShowdown :: Winners -> Bool
isMultiPlayerShowdown (MultiPlayerShowdown _) = True
isMultiPlayerShowdown _ = False

allButOneAllIn :: Game -> Bool
allButOneAllIn = (== 1) . countPlayersNotAllIn

everyoneAllIn :: Game -> Bool
everyoneAllIn = (== 0) . countPlayersNotAllIn

countPlayersNotAllIn :: Game -> Int
countPlayersNotAllIn game@Game {..}
  | numPlayersIn < 2 = 0
  | otherwise = numPlayersIn - numPlayersAllIn
  where
    numPlayersIn = length $ getActivePlayers _players
    numPlayersAllIn =
      length $ filter (\Player {..} -> _playerState == In && _chips == 0) _players
