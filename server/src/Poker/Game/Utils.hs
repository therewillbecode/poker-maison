{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game.Utils where

import Control.Lens ((^.))
import Data.Bool (bool)
import Data.Foldable (find)
import Data.List (elemIndex,findIndex,  find)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import Poker.Types
import System.Random (Random (randomR), RandomGen)



isSatIn :: Player -> Bool
isSatIn (SatOutP _) = False 
isSatIn _ = True

isSatOut :: Player -> Bool
isSatOut = not . isSatIn

isAllIn (InHandP InHandPlayer{..}) = _status == CannotAct IsAllIn
isAllIn _ = False

getPockets (InHandP p) = p ^. pockets
getPockets _ = Nothing


getCommitted (PreHandP p) = p ^. committed
getCommitted  (SatOutP p@SatOutPlayer{}) = CommittedChips 0
getCommitted (InHandP p) = p ^. committed

getCurrBet (InHandP p) =  p ^. currBet
getCurrBet _ = Chips 0


getChips (PreHandP ( p@PreHandPlayer{})) = p ^. chips
getChips (SatOutP p@SatOutPlayer{}) = p ^. chips
getChips (InHandP p) = p ^. chips

getPlayerName (PreHandP (p@PreHandPlayer{})) = p ^. playerName

getPlayerName (SatOutP p@SatOutPlayer{}) = p ^. playerName

getPlayerName (InHandP p) = p ^. playerName
getPlayerName (InHandP  p) = p ^. playerName
getPlayerName (InHandP  p) = p ^. playerName

isFolded :: Player -> Bool
isFolded (InHandP InHandPlayer{..}) = _status == CannotAct HasFolded
isFolded _ = False

inHandAndNotFolded (InHandP InHandPlayer{..}) = _status == CannotAct HasFolded
inHandAndNotFolded _ = False

canPlayerAct :: InHandPlayer -> Bool
canPlayerAct (InHandP InHandPlayer{..}) = _status == CanAct
canPlayerAct (PreHandP PreHandPlayer{..}) = _status == NoBlindRequired
canPlayerAct _ = False

shouldDeal (PreHandP ( p@PreHandPlayer{})) = True
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
getActivePlayers :: [InHandPlayer] -> [InHandPlayer]
getActivePlayers = filter canPlayerAct

filterPlayersWithLtChips :: Int -> [Player] -> [Player]
filterPlayersWithLtChips count =
  filter
    ( \p ->
      getChips p >= Chips count
    )

--filterSatOutPlayers :: [Player] -> [Player]
--filterSatOutPlayers = filter isSatOut

countActive :: [InHandPlayer] -> Int
countActive = length . getActivePlayers

--actedThisTurn :: PlayerStatus -> HasActedThisStreet
--actedThisTurn (InHand (CanAct mbLastAction)) = isJust mbLastAction

canAnyPlayerAct :: [InHandPlayer] -> Bool
canAnyPlayerAct ps = any canPlayerAct ps

handStatus :: [InHandPlayer] -> HandStatus
handStatus ps
  | allButOneFolded ps = EveryoneFolded
  | playersNotAllIn ps == 1 = EveryoneAllIn
  | canAnyPlayerAct ps = AwaitingPlayerAction
  | not (canAnyPlayerAct ps) = NotAwaitingPlayerAction
  | otherwise = error "undhandled guard"

allButOneAllIn :: [Player] -> Bool
allButOneAllIn = (== 1) . playersNotAllIn

playersNotAllIn :: [InHandPlayer] -> Int
playersNotAllIn ps
  | numPlayersIn < 2 = 0
  | otherwise = numPlayersIn - numPlayersAllIn
  where
    numPlayersIn = length $ getActivePlayers ps
    numPlayersAllIn =
      length $ filter isAllIn ps



-- The game should go straight to showdown if all but one players is In hand
allButOneFolded :: [InHandPlayer] -> Bool
allButOneFolded ps = length inHandAndNotFolded' <= 1
  where
    inHandAndNotFolded' = filter inHandAndNotFolded ps



-- player position is the order of a given player in the set of all players with a
-- playerStatus of In or in other words the players that are both sat at the table and active
-- return Nothing if the given playerName is not sat at table
--getPlayerPosition :: [PlayerName] -> PlayerName -> Maybe Int
--getPlayerPosition players playerName = playerName `elemIndex` players

--getPlayerPosition' :: PlayerName -> p -> Maybe Int
--getPlayerPosition' playerName = 
--  flip getPlayerPosition playerName . getPlayerNames

getGameStage :: HandInProgress -> Street
getGameStage game = game ^. street



--getGamePlayer ::_-> PlayerName -> Maybe Player
--getGamePlayer game playerName =
 -- find (\p-> getPlayerName p == playerName) $ getGamePlayers game

--getGamePlayerState :: Game -> PlayerName -> Maybe PlayerStatus
--getGamePlayerState game playerName = do
--  PlayerInfo {..} <- getGamePlayer game playerName
--  return _playerStatus

--getGamePlayerNames :: _ -> [Text]
--getPlayerNames game = getPlayerName <$> getGamePlayers game


--getPlayerChipCounts :: Game -> [(Text, Int)]
--getPlayerChipCounts Game {..} =
 -- (\PlayerInfo {..} -> (_playerName, unChips _chips)) <$> _players

--getInHandPlayerNames :: [InHandPlayer] -> [Text]
--getInHandPlayerNames players = getPlayerName <$> players
--

getPlayers :: HasPlayers g [p] => g -> [p]
getPlayers g = g ^. players

getPlayerPos :: HasPlayerName p Text => Text -> [p] -> Maybe Int
getPlayerPos pName = findIndex $ \p -> (p ^. playerName) == pName

getPlayer :: HasPlayerName p Text => Text -> [p] -> Maybe p
getPlayer pName = find $ \p -> (p ^. playerName) == pName


-- Nothing for currentPosToAct during Predeal means that the first blind
-- can be posted from any position as this is the first blind to get a new game started
-- on the otherhand a value of Just pos means that pos is the position that we require a blind to
-- be posted from next as a game is underway.
inPositionToAct :: PlayerName -> HandInProgress -> Bool
inPositionToAct pName HandInProgress {..} =
  case getPlayerPos pName _players of
    Nothing -> False
    Just pos -> pos == _currentPosToAct
 

maximums :: Ord a => [(a, b)] -> [(a, b)]
maximums [] = []
maximums (x : xs) = foldl f [x] xs
  where
    f ys y = case fst (head ys) `compare` fst y of
      GT -> ys
      EQ -> y : ys
      LT -> [y]
