{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poker.Generators where

import Control.Lens
  ( Field2 (_2),
    FunctorWithIndex (imap),
    (%~),
    (.~),
    (^.),
  )
import Control.Monad (Monad (return), replicateM)
import Control.Monad.State (Monad (return), replicateM)
import Data.Either ()
import qualified Data.List as List
import Data.Maybe (Maybe (..))
import Data.Proxy ()
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Traversable (mapAccumR)
import Data.Tuple (fst, swap)
import qualified Data.Vector as V
import Debug.Trace ()
import GHC.Enum (Enum (fromEnum))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Poker.Game.Game (getWinners, nextIPlayerToAct)
import Poker.Game.Utils
  ( getActivePlayers,
    getPlayersSatIn,
    initialDeck,
  )
import Poker.Types
  ( Card (suit),
    Deck (..),
    Game (..),
    Player (..),
    PlayerName,
    PlayerState (..),
    PocketCards (..),
    SatInState (..),
    Street (..),
    Suit (Diamonds, Spades),
    Winners (NoWinners),
    bet,
    playerName,
    unDeck,
  )
import Prelude

allPStates :: [PlayerState]
allPStates = [SatOut, SatIn Folded, SatIn NotFolded]

allPStreets :: [Street]
allPStreets = [PreDeal ..]

numBoardCards :: Street -> Int
numBoardCards =
  \case
    PreDeal -> 0
    PreFlop -> 0
    Flop -> 3
    Turn -> 4
    River -> 5
    Showdown -> 5

genShuffledCards :: Int -> Gen [Card]
genShuffledCards n = do
  cs <- Gen.shuffle $ unDeck initialDeck
  return $ take n cs

genShuffledDeck :: Gen Deck
genShuffledDeck = do
  cs <- Gen.shuffle $ unDeck initialDeck
  return $ Deck cs

genSuit :: Gen Suit
genSuit = Gen.enum Diamonds Spades

genSameSuitCards :: Int -> Gen [Card]
genSameSuitCards n = do
  suit' <- genSuit
  cs <- Gen.shuffle $ filter ((== suit') . suit) (unDeck initialDeck)
  return $ take n cs

genDealPockets :: [Card] -> Gen (Maybe PocketCards, [Card])
genDealPockets cs = do
  let ([c1, c2], remainingCs) = splitAt 2 cs
  return (Just $ PocketCards c1 c2, remainingCs)

genNoPockets :: [Card] -> Gen (Maybe PocketCards, [Card])
genNoPockets cs = return (Nothing, cs)

genPlayers :: Street -> Int -> [PlayerState] -> Int -> [Card] -> Gen ([Player], [Card])
genPlayers street' requiredInPlayers possibleStates playerCount cs = do
  ps <- replicateM playerCount $ do
    pState <- Gen.element possibleStates
    genPlayer street' pState "player" Nothing
  if activesCount ps < requiredInPlayers || street' `elem` actionStages && satInCount ps < 2
    then Gen.discard
    else return $ swap $ dealPlayersGen ps cs
  where
    actionStages = [PreFlop, Flop, Turn, River]
    activesCount ps = length $ getActivePlayers ps
    satInCount ps = length $ getPlayersSatIn ps

dealPlayersGen :: [Player] -> [Card] -> ([Card], [Player])
dealPlayersGen ps cs =
  _2 %~ nameByPos $
    mapAccumR
      ( \cs p ->
          let (maybeDealtP, remainingCs') = dealPlayer cs p
           in (remainingCs', maybeDealtP)
      )
      cs
      ps
  where
    newName pos = playerName .~ "player" <> T.pack (show pos)
    nameByPos ps = imap newName ps
    dealPlayer cs plyr@Player {..}
      | _playerState == SatOut = (plyr, cs)
      | otherwise = (,) Player {_pockets = Just $ PocketCards c1 c2, ..} remainingCs'
      where
        ([c1, c2], remainingCs') = splitAt 2 cs

genPlayer' :: Street -> [PlayerState] -> Int -> [Card] -> Gen (Player, [Card])
genPlayer' street' possibleStates position cs = do
  pState <- Gen.element possibleStates
  let shouldDeal = pState /= SatOut || null cs
  (pocketCs, remainingCs) <- if shouldDeal then genDealPockets cs else genNoPockets cs
  p <- genPlayer street' pState pName pocketCs
  return (p, remainingCs)
  where
    pName = "player" <> T.pack (show position)

-- if given Just cards then will deal player (as long as not sat out) and return the remaining cards
--
-- minChips is calculated to reflect fact that a player can't fold if was all in (no action possible when chips 0)
-- a player who has state set to Folded and has 0 chips is not a valid player state
genPlayer :: Street -> PlayerState -> PlayerName -> Maybe PocketCards -> Gen Player
genPlayer street' _playerState _playerName _pockets = do
  _chips <- Gen.int $ Range.linear minChips 10000
  _committed <- if _playerState == SatOut then Gen.constant 0 else Gen.int $ Range.linear 0 10000
  _bet <- if street' == PreDeal then Gen.constant 0 else Gen.int $ Range.linear 0 _committed
  _actedThisTurn <- if _playerState == SatOut then Gen.constant False else Gen.bool
  return Player {..}
  where
    minChips = if _playerState == SatIn Folded then 1 else 0
    _possibleActions = []

genGame :: [Street] -> [PlayerState] -> Gen Game
genGame possibleStreets pStates = do
  _street <- Gen.element possibleStreets
  let d@(Deck cs) = initialDeck
  let boardCount = numBoardCards _street
      (boardCards, remainingCs) = splitAt boardCount cs
  _smallBlind <- Gen.int $ Range.linear 1 100
  _maxPlayers <- Gen.int $ Range.linear 2 9
  playerCount <- Gen.int $ Range.linear 2 _maxPlayers
  let requiredInPlyrs = if _street == Showdown then 2 else 0
  (_players, remainingCs') <- genPlayers _street requiredInPlyrs pStates playerCount remainingCs
  let _waitlist = []
      _bigBlind = _smallBlind * 2
      _maxBuyInChips = _bigBlind * 200
      _minBuyInChips = _bigBlind * 100
      (_board, remainingCs'') = splitAt (numBoardCards _street) remainingCs'
      _deck = Deck remainingCs''
      betsThisRound = (^. bet) <$> _players
      _maxBet = maximum betsThisRound
      betSum = sum betsThisRound
      playerCount = length _players
      _winners = if _street == Showdown then getWinners Game {..} else NoWinners
  _pot <- Gen.int $ Range.linear betSum (betSum * fromEnum _street)
  _dealer <- Gen.int $ Range.linear 0 (playerCount - 1)
  _currentPosToAct' <- Gen.int $ Range.linear 0 (playerCount - 1)
  let g'' = Game {..}
      _currentPosToAct = fst <$> nextIPlayerToAct g'' (Just _dealer)
  return Game {..}
