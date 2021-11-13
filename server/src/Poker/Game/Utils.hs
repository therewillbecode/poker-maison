{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game.Utils where

import Control.Lens ((^.))
import Data.Bool (bool)
import Data.Foldable (find)
import Data.List (elemIndex, find)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import Poker.Types
import System.Random (Random (randomR), RandomGen)


isSatIn :: Player -> Bool
isSatIn  (PreHandP (SatOutP SatOutPlayer{})) = False 
isSatIn _ = True

isSatOut :: Player -> Bool
isSatOut = not . isSatIn

isAllIn (InHandP (AllInP p)) = True
isAllIn _ = False


getPockets (InHandP (CanActP p)) = p ^. pockets
getPockets (InHandP (FoldedP p)) = p ^. pockets
getPockets (InHandP (AllInP p)) = p ^. pockets
getPockets _ =Nothing

hasFolded (InHandP (FoldedP _)) = True
hasFolded (InHandP (AllInP _)) = False 

getCommitted (PreHandP (NeedsBlindP p@BlindRequiredPlayer{})) = CommittedChips 0
getCommitted (PreHandP (NoBlindNeededP p@NoBlindRequiredPlayer{})) =  CommittedChips 0
getCommitted (PreHandP (HasPostedBlindP p@HasPostedBlindPlayer{})) = p ^. committed
getCommitted (PreHandP (SatOutP p@SatOutPlayer{})) = CommittedChips 0
getCommitted (InHandP (CanActP p)) = p ^. committed
getCommitted (InHandP (FoldedP p)) = p ^. committed
getCommitted (InHandP (AllInP p)) = p ^. committed

getCurrBet (InHandP (CanActP p)) =  p ^. currBet
getCurrBet (InHandP (FoldedP p)) = p ^. currBet
getCurrBet (InHandP (AllInP p)) =  p ^. currBet
getCurrBet _ = Chips 0

getChips (PreHandP (NeedsBlindP p@BlindRequiredPlayer{})) = p ^. chips
getChips (PreHandP (NoBlindNeededP p@NoBlindRequiredPlayer{})) = p ^. chips
getChips (PreHandP (HasPostedBlindP p@HasPostedBlindPlayer{})) = p ^. chips
getChips (PreHandP (SatOutP p@SatOutPlayer{})) = p ^. chips
getChips (InHandP (CanActP p)) = p ^. chips
getChips (InHandP (FoldedP p)) = p ^. chips
getChips (InHandP (AllInP _)) = Chips 0

getPlayerName (PreHandP (NeedsBlindP p@BlindRequiredPlayer{})) = p ^. playerName
getPlayerName (PreHandP (NoBlindNeededP p@NoBlindRequiredPlayer{})) = p ^. playerName
getPlayerName (PreHandP (HasPostedBlindP p@HasPostedBlindPlayer{})) = p ^. playerName
getPlayerName (PreHandP (SatOutP p@SatOutPlayer{})) = p ^. playerName
getPlayerName (InHandP (CanActP p)) = p ^. playerName
getPlayerName (InHandP (FoldedP p)) = p ^. playerName
getPlayerName (InHandP (AllInP p)) = p ^. playerName


inHandAndNotFolded (InHandP (AllInP _))  = True
inHandAndNotFolded (InHandP (CanActP _)) = True
inHandAndNotFolded _ = False

canPlayerAct (InHandP (CanActP p)) = PlayerCanAct
canPlayerAct _ = PlayerCannotAct

shouldDeal (PreHandP (NoBlindNeededP p@NoBlindRequiredPlayer{})) = True
shouldDeal (PreHandP (HasPostedBlindP p@HasPostedBlindPlayer{})) = True
shouldDeal _ = False


-- | A standard deck of cards.
initialDeck :: Deck
initialDeck = Deck $ Card <$> [minBound ..] <*> [minBound ..]

-- Get a shuffled deck of cards.
shuffledDeck :: RandomGen g => g -> Deck
shuffledDeck gen = Deck <$> fst $ shuffle gen (unDeck initialDeck)

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) =
  ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

-- shuffle using the Fisher Yates algorithm
shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen l =
  toElems $
    foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (M.elems x, y)
    numerate = zip [1 ..]
    initial x gen = (M.singleton 0 x, gen)

modInc :: Int -> Int -> Int -> Int
modInc incAmount num modulo
  | incNum > modulo = 0
  | otherwise = incNum
  where
    incNum = num + incAmount
    modInc = incNum `mod` modulo

modDec :: Int -> Int -> Int
modDec num modulo
  | decNum < modulo = 0
  | otherwise = decNum
  where
    decNum = num - 1
    modInc = decNum `mod` modulo

-- return players which have the ability to make further moves i.e not all in or folded
-- the distinction between sat in and active is important
-- if a player is sat out then there has been no historical participation in this hand
-- as there can be no future participation in this hand
-- whereas sat in means that the player has at the very least had some historical participation
-- in the current hand
getActivePlayers :: [Player] -> [Player]
getActivePlayers = filter (((==) PlayerCanAct) . canPlayerAct)

filterPlayersWithLtChips :: Int -> [Player] -> [Player]
filterPlayersWithLtChips count =
  filter
    ( \p ->
      getChips p >= Chips count
    )

filterSatOutPlayers :: [Player] -> [Player]
filterSatOutPlayers = filter isSatOut

countActive :: [Player] -> Int
countActive = length . getActivePlayers

--actedThisTurn :: PlayerStatus -> HasActedThisStreet
--actedThisTurn (InHand (CanAct mbLastAction)) = isJust mbLastAction


canPlayersAct :: Functor f => f Player -> f CanPlayerAct
canPlayersAct ps = canPlayerAct <$> ps

canAnyPlayerAct :: [Player] -> Bool
canAnyPlayerAct = elem PlayerCanAct . canPlayersAct

bettingActionStatus :: [Player] -> BettingAction
bettingActionStatus ps
  | allButOneFolded ps = EveryoneFolded
  | playersNotAllIn ps == 1 = EveryoneAllIn
  | canAnyPlayerAct ps = AwaitingPlayerAction
  | not (canAnyPlayerAct ps) = NotAwaitingPlayerAction
  | otherwise = error "undhandled guard"

allButOneAllIn :: [Player] -> Bool
allButOneAllIn = (== 1) . playersNotAllIn

playersNotAllIn :: [Player] -> Int
playersNotAllIn ps
  | numPlayersIn < 2 = 0
  | otherwise = numPlayersIn - numPlayersAllIn
  where
    numPlayersIn = length $ getActivePlayers ps
    numPlayersAllIn =
      length $ filter isAllIn ps



-- The game should go straight to showdown if all but one players is In hand
allButOneFolded :: [Player] -> Bool
allButOneFolded ps = length inHandAndNotFolded' <= 1
  where
    inHandAndNotFolded' = filter inHandAndNotFolded ps

-- get all players who are not currently sat out
getPlayersSatIn :: [Player] -> [Player]
getPlayersSatIn = filter isSatOut


-- player position is the order of a given player in the set of all players with a
-- playerStatus of In or in other words the players that are both sat at the table and active
-- return Nothing if the given playerName is not sat at table
getPlayerPosition :: [PlayerName] -> PlayerName -> Maybe Int
getPlayerPosition playersSatIn playerName = playerName `elemIndex` playersSatIn

getPlayerPosition' :: PlayerName -> [Player] -> Maybe Int
getPlayerPosition' playerName = flip getPlayerPosition playerName . getPlayerNames . getPlayersSatIn

getGameStage :: Game -> Street
getGameStage game = game ^. street

getGamePlayers :: Game -> [Player]
getGamePlayers game = game ^. players

getGamePlayer :: Game -> PlayerName -> Maybe Player
getGamePlayer game playerName =
  find (\p-> getPlayerName p == playerName) $ _players game

--getGamePlayerState :: Game -> PlayerName -> Maybe PlayerStatus
--getGamePlayerState game playerName = do
--  PlayerInfo {..} <- getGamePlayer game playerName
--  return _playerStatus

getGamePlayerNames :: Game -> [Text]
getGamePlayerNames game = getPlayerName <$> _players game


--getPlayerChipCounts :: Game -> [(Text, Int)]
--getPlayerChipCounts Game {..} =
 -- (\PlayerInfo {..} -> (_playerName, unChips _chips)) <$> _players

getPlayerNames :: [Player] -> [Text]
getPlayerNames players = getPlayerName <$> players

-- Nothing for currentPosToAct during Predeal means that the first blind
-- can be posted from any position as this is the first blind to get a new game started
-- on the otherhand a value of Just pos means that pos is the position that we require a blind to
-- be posted from next as a game is underway.
inPositionToAct :: PlayerName -> Game -> Bool
inPositionToAct playerName Game {..} =
  case playerPos of
    Nothing -> False
    Just pos -> case _currentPosToAct of
      Nothing -> _street == PreDeal -- Wheareas Nothing during Predeal means anyone can act
      -- Nothing in currentPostToAct field after predeal means no player can act.
      Just posToAct -> pos == posToAct
  where
    playerPos = getPlayerPosition' playerName _players

maximums :: Ord a => [(a, b)] -> [(a, b)]
maximums [] = []
maximums (x : xs) = foldl f [x] xs
  where
    f ys y = case fst (head ys) `compare` fst y of
      GT -> ys
      EQ -> y : ys
      LT -> [y]
