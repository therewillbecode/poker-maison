{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game.Blinds where

import Control.Lens ((%~), (&))
import Control.Monad.State ()
import Data.Char (toLower)
import Data.List (all, find, length, splitAt, tail, zip, zipWith)
import qualified Data.List.Safe as Safe
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Poker.Game.Utils
  ( getGamePlayer,
    getPlayerNames,
    getPlayerPosition,
    modInc,
  )
import Poker.Types
  ( Blind (..),
    Game (..),
    Player (..),
    PlayerName,
    PlayerState (..),
    SatInState (..),
    Street (PreDeal),
    players,
  )
import Text.Read (readMaybe)
import Prelude

-- Gets the player position where the next required blind is
-- This function always us timeout players in the blinds stage if they don't post
-- the required blinds in order
--
-- TODO - abstract out the duplication from nextPosToAct
getPosNextBlind :: Int -> Game -> Int
getPosNextBlind currIx game@Game {..} = nextIx
  where
    iplayers = zip [0 ..] _players
    iplayers' = let (a, b) = splitAt currIx iplayers in b <> a
    (nextIx, nextPlayer) =
      fromJust $
        find
          (\(_, p@Player {..}) -> blindRequiredByPlayer game _playerName /= NoBlind)
          (tail iplayers')

haveRequiredBlindsBeenPosted :: Game -> Bool
haveRequiredBlindsBeenPosted game@Game {..} =
  all (== True) $
    zipWith
      ( \requiredBlind Player {..} -> case requiredBlind of
          NoBlind -> True
          Big -> _committed == _bigBlind
          Small -> _committed == _smallBlind
      )
      requiredBlinds
      _players
  where
    requiredBlinds = getRequiredBlinds game

getRequiredBlinds :: Game -> [Blind]
getRequiredBlinds game@Game {..}
  | _street /= PreDeal = []
  | otherwise = blindRequiredByPlayer game <$> getPlayerNames _players

-- We use the list of required blinds to calculate if a player has posted
-- chips sufficient to be "In" for this hand.
activatePlayersWhenNoBlindNeeded :: [Blind] -> [Player] -> [Player]
activatePlayersWhenNoBlindNeeded = zipWith updatePlayer
  where
    updatePlayer blindReq Player {..} =
      Player
        { _playerState = if blindReq == NoBlind then SatIn NotFolded else _playerState,
          ..
        }

-- Sets player state to in if they don't need to post blind
updatePlayersInHand :: Game -> Game
updatePlayersInHand game =
  game & (players %~ activatePlayersWhenNoBlindNeeded (getRequiredBlinds game))

getSmallBlindPosition :: [Text] -> Int -> Int
getSmallBlindPosition playersSatIn dealerPos =
  if length playersSatIn == 2
    then dealerPos
    else modInc incAmount dealerPos (length playersSatIn - 1)
  where
    incAmount = 1

-- if a player does not post their blind at the appropriate time then their state will be changed to
-- SatOut signifying that they have a seat but are now sat out
-- blind is required either if player is sitting in bigBlind or smallBlind position relative to dealer
-- or if their current playerState is set to Out
-- If no blind is required for the player to remain In for the next hand then we will return Nothing
blindRequiredByPlayer :: Game -> PlayerName -> Blind
blindRequiredByPlayer game playerName
  | -- SO IS THIS NEEDED FOR BOTS?? IT BREAKS NON BOTS
    --  | length (_players game) < 2 || _street game /= PreDeal = NoBlind
    playerPosition == smallBlindPos =
    Small
  | playerPosition == bigBlindPos = Big
  | otherwise = NoBlind
  where
    player = fromJust $ getGamePlayer game playerName
    playerNames = getPlayerNames (_players game)
    playerPosition = fromJust $ getPlayerPosition playerNames playerName
    smallBlindPos = getSmallBlindPosition playerNames (_dealer game)
    incAmount = 1
    bigBlindPos = modInc incAmount smallBlindPos (length playerNames - 1)
