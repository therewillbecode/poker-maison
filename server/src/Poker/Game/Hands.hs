{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Poker.Game.Hands where

import Data.Function (on)
import Data.List (foldl', groupBy, nubBy, sort, sortBy)
import Data.Ord (comparing)
import Poker.Types
  ( Card (rank, suit),
    HandRank (..),
    Rank (Ace, Five, Four, Three, Two),
  )

type RankGroup = Int

value :: [Card] -> (HandRank, [Card])
value hand = maybe (ifNotFlush hand) ifFlush (maybeFlush hand)

ifNotFlush :: [Card] -> (HandRank, [Card])
ifNotFlush hand = maybe (checkGroups hand) (Straight,) (maybeStraight hand)

ifFlush :: [Card] -> (HandRank, [Card])
ifFlush hand =
  maybe (Flush, take 5 hand) (StraightFlush,) (maybeStraight hand)

lastNelems :: Int -> [a] -> [a]
lastNelems n xs = foldl' (const . drop 1) xs (drop n xs)

maybeFlush :: [Card] -> Maybe [Card]
maybeFlush cs
  | length cs' >= 5 = Just cs'
  | otherwise = Nothing
  where
    sortBySuit = sortBy (comparing suit <> flip compare)
    groupBySuit = groupBy ((==) `on` suit)
    cs' = head $ sortByLength $ groupBySuit $ sortBySuit cs

maybeStraight :: [Card] -> Maybe [Card]
maybeStraight cards
  | length cs'' >= 5 = Just (lastNelems 5 cs'')
  | otherwise = maybeWheel cardsUniqRanks
  where
    cardsUniqRanks = nubBy ((==) `on` rank) cards
    cs'' = head $ sortByLength $ groupBySuccCards $ sort cardsUniqRanks

maybeWheel :: [Card] -> Maybe [Card]
maybeWheel cards
  | length filteredCards == 5 = Just filteredCards
  | otherwise = Nothing
  where
    filteredCards =
      (flip elem [Ace, Two, Three, Four, Five] . rank) `filter` cards

checkGroups :: [Card] -> (HandRank, [Card])
checkGroups hand = (hRank, cards)
  where
    groups = sortByLength $ groupBy ((==) `on` rank) $ sort hand
    cards = take 5 $ concat groups
    groupedRankLengths = length <$> groups
    hRank = evalGroupedRanks groupedRankLengths

evalGroupedRanks :: [RankGroup] -> HandRank
evalGroupedRanks = \case
  (4 : _) -> Quads
  (3 : 2 : _) -> FullHouse
  (3 : _) -> Trips
  (2 : 2 : _) -> TwoPair
  (2 : _) -> Pair
  _ -> HighCard

groupBySuccCards :: [Card] -> [[Card]]
groupBySuccCards = foldr f []
  where
    f :: Card -> [[Card]] -> [[Card]]
    f a [] = [[a]]
    f a xs@(x : xs')
      | succ (rank a) == rank (head x) = (a : x) : xs'
      | otherwise = [a] : xs

sortByLength :: Ord a => [[a]] -> [[a]]
sortByLength = sortBy (flip (comparing length) <> flip compare)
