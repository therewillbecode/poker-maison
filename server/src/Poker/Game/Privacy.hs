{-
  Logic for excluding sensitive game data from game state.
-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game.Privacy where

import Control.Lens ((%~), (&), (.~))
import Data.Text (Text)
import Poker.Game.Game (canPubliciseActivesCards)
import Poker.Types

-- For players that are sat in game
excludeOtherPlayerCards :: PlayerName -> Game -> Game
excludeOtherPlayerCards playerName = excludePrivateCards $ Just playerName

--  -- For spectators who aren't in game
excludeAllPlayerCards :: Game -> Game
excludeAllPlayerCards = excludePrivateCards Nothing

-- Exclude player cards and Deck so spectators can't see private cards.
--
-- Takes an optional playerName. If no playerName is given then all private
-- cards are excluded. However if playerName is given then their cards
-- will not be excluded.
--
-- So if a game update is going to be sent to a user then we pass in his playerName
-- so that information that is private to him is not excluded from the
-- Game state (his pocket cards)
--
-- If everyone in the game is AllIn then their pocket cards should all be visible.
--
---- We show all active players cards in the case of every active player being all in
-- or during the final showdown stage of the game
--
-- If they are
-- then all the pocket cards held by players whose _playerStatus
-- is set to In (active and) are public and therefore not removed.
excludePrivateCards :: Maybe PlayerName -> Game -> Game
excludePrivateCards maybePlayerName game =
  game & (players %~ (<$>) pocketCardsPrivacyModifier) . (deck .~ Deck [])
  where
    showAllActivesCards = canPubliciseActivesCards game
    pocketCardsPrivacyModifier =
      maybe
        (updatePocketCardsForSpectator showAllActivesCards)
        (updatePocketCardsForPlayer showAllActivesCards)
        maybePlayerName

updatePocketCardsForSpectator :: Bool -> (Player -> Player)
updatePocketCardsForSpectator showAllActivesCards
  | showAllActivesCards = \player@Player {..} ->
    if _playerStatus /= InHand Folded then player else Player {_pockets = Nothing, ..}
  | otherwise = \Player {..} -> Player {_pockets = Nothing, ..}

updatePocketCardsForPlayer :: Bool -> PlayerName -> (Player -> Player)
updatePocketCardsForPlayer showAllActivesCards playerName
  | showAllActivesCards = \player@Player {..} ->
    if (_playerStatus /= InHand Folded) || (_playerName == playerName)
      then player
      else Player {_pockets = Nothing, ..}
  | otherwise = \player@Player {..} ->
    if _playerName == playerName
      then player
      else Player {_pockets = Nothing, ..}
