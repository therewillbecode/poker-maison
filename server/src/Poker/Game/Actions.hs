{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game.Actions where

import Control.Lens ((%~), (&), (+~), (-~), (.~), (<>~), (^.))
import Control.Monad.State (Functor)
import Data.Bool (Bool (False, True), bool)
import Data.Char (toLower)
import Data.List (filter, find, findIndex, sum)
import qualified Data.List.Safe as Safe
import Data.Maybe (fromJust)
import Poker.Game.Blinds (getPosNextBlind)
import Poker.Game.Game 
import Poker.Types
import Text.Read (readMaybe)
import Prelude
import Poker.Game.Utils 

-- separately enforce this validation rule has enough chips
--   hasEnoughChips = unChips chips' > betSize
--      betAmount = bool (unChips chips') betSize hasEnoughChips

makeBet :: Bool -> Chips -> PlayerName -> HandInProgress -> Game
makeBet isCall betSize pName game@Game {..} =
  updateMaxBet betSize game
    & (players %~ placePlayerBet)
      . (currentPosToAct %~ nextPosToAct _players)
      . (pot +~ betSize)
  where
    placePlayerBet = (<$>) $
      \p@PlayerInfo {..} ->
        if _playerName == pName
          then placeBet isCall betSize p
          else p

-- Update table maxBet and pot as well as player state and chip count
placeBet :: Bool -> Chips -> Player -> Player
placeBet isCall betSize plyr = undefined
  --let chips' = getChips plyr 
  -- in plyr
  --      & (chips -~ betSize)
  --        . (bet <>~ betSize)
  --        . (committed <>~ CommittedChips (unChips betSize))
  --        . ( playerStatus
  --              %~ nextPlayerStatus
  --                chips'
  --                (bool Call (Bet betSize) isCall)
  --          )

--nextPlayerStatus :: Chips -> Action -> PlayerStatus -> PlayerStatus
--nextPlayerStatus (Chips 0) _ _ = InHand AllIn
--nextPlayerStatus _ Fold _ = InHand Folded
--nextPlayerStatus _ Check playerStatus =
--  InHand $ CanAct Checked
--nextPlayerStatus _ Call playerStatus =
--  InHand $ CanAct $ MadeBet HasCalled
--nextPlayerStatus _ (Bet size) playerStatus =
--  InHand $ CanAct $ MadeBet $ HasBet size
--nextPlayerStatus _ (Raise size) playerStatus =
--  InHand $ CanAct $ MadeBet $ HasRaised size
--nextPlayerStatus _ (PostBlind blind) playerStatus =
--  SatIn HasPlayedLastHand $ PostedBlind blind
--nextPlayerStatus _ SitIn playerStatus =
--  SatIn HasPlayedLastHand NotPostedBlind
--nextPlayerStatus _ Timeout playerStatus = playerStatus
--nextPlayerStatus _ SitOut playerStatus = SatOut
--nextPlayerStatus _ _ playerStatus = playerStatus

--
--markActed :: Action -> PlayerInfo -> PlayerInfo
--markActed action p@PlayerInfo {..} =
--  p & (playerStatus %~ newPlayerStatus _chips action)

updateMaxBet :: Chips -> Game -> Game
updateMaxBet amount = maxBet %~ max amount

markInForHand :: Player -> Player
markInForHand p = undefined
  -- if getChips p == 0 
  -- then p
  -- else p & hasActed .~ NotActedThisTurn  

-- Will increment the game's current position to act to the next position
-- where a blind is required. Skipping players that do not have to post blinds
-- during the PreDeal phase of the game is desirable as by definition
-- the only possible players actions during the PreDeal phase are to either:
--   1. Sit out of the game
--   2. Post a blind.
postBlind :: Blind -> PlayerName -> Game -> Game
postBlind blind pName game@Game {..} = undefined
  ---- hack because I dont know lens
  --let game' = makeBet False (Chips blindValue) pName game
  -- in game'
  --      & (pot +~ Chips blindValue)
  --        . (currentPosToAct .~ pure nextRequiredBlindPos)
  --        . (maxBet .~ newMaxBet)
  --where
  --  isFirstBlind = sum ((\p -> getCurrBet p) <$> _players) == 0
  --  gamePlayerNames = (\p -> getPlayerName p) <$> _players
  --  blindValue = if blind == SmallBlind then _smallBlind else _bigBlind
  --  newMaxBet = Chips $ if blindValue > unChips _maxBet then blindValue else unChips _maxBet
  --  positionOfBlindPoster = fromJust $ findIndex ((== pName) . (^. playerName)) _players
  --  nextRequiredBlindPos = getPosNextBlind positionOfBlindPoster game

foldCards :: PlayerName -> Game -> Game
foldCards pName game@Game {..} =
  game & (players .~ newPlayers) . (currentPosToAct %~ nextPosToAct _players)
  where
    newPlayers =
      ( \p ->
          if getPlayerName p == pName
            then  foldPlayer p
            else p
      )
        <$> _players

foldPlayer :: InHandPlayer -> InHandPlayer
foldPlayer = status .~ CannotAct HasFolded

call :: PlayerName -> Game -> Game
call pName game@Game {..} = undefined
  ---- hack because i dont know lens
  --let game' = makeBet True callAmount pName game
  -- in game'
  --      & (currentPosToAct %~ nextPosToAct _players)
  --        . (pot +~ callAmount)
  --where
  --  player = fromJust $ find (\p -> getPlayerName p == pName) _players --horrible performance use map for players
  --  callAmount =
  --    let maxBetShortfall = _maxBet - (player ^. bet)
  --        playerChips = getChips player
  --     in if maxBetShortfall > playerChips
  --          then playerChips
  --          else maxBetShortfall

check :: PlayerName -> Game -> Game
check pName game@Game {..} = undefined
  --game & (players .~ newPlayers) . (currentPosToAct %~ nextPosToAct _players)
  --where
  --  newPlayers =
  --    ( \p ->
  --        if getPlayerName p == pName
  --          then p & playerStatus %~ nextPlayerStatus (getChips p) Check
  --          else p
  --    )
  --      <$> _players

-- Sets state of a given player to SatOut (sat-out)
-- In order to sit in again the player must post a blind
sitOut :: PlayerName -> Game -> Game
sitOut plyrName =
  players
    %~ (<$>)
      ( \p ->
          if getPlayerName p == plyrName
            then SatOutP $ sitPlayerOut p
            else p
      )

sitIn :: PlayerName -> Game -> Game
sitIn plyrName g =
  g &  (players
    %~ (<$>)
      ( \p ->
          if getPlayerName p == plyrName
            then
              PreHandP $ newPreHandPlayer g p
            else p
      ))


seatPlayer :: Player -> Game -> Game
seatPlayer plyr = players <>~ pure plyr

joinWaitlist :: Player -> Game -> Game
joinWaitlist p = waitlist %~ (:) (getPlayerName p)

leaveSeat :: PlayerName -> Game -> Game
leaveSeat pName =
  players %~ filter (\p -> getPlayerName p /= pName)
