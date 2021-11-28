{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poker.HandSpec where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Poker.Game.Hands (maybeFlush, value)
import Poker.GeneratorsSpec (genSameSuitCards, genShuffledCards)
import Test.Hspec (describe, it)
import Test.Hspec.Hedgehog
  ( PropertyT,
    diff,
    forAll,
    hedgehog,
    modifyMaxDiscardRatio,
    (/==),
    (===),
  )

spec = do
  describe "value" $ do
    it "Number of cards before and after valuation is involutive" $
      hedgehog $ do
        sevenCards <- forAll (genShuffledCards 7)
        let (_, cs) = value sevenCards
        length cs === 5

  it "7 suited cards always a flush" $
    hedgehog $ do
      cs <- forAll $ genSameSuitCards 7
      isJust (maybeFlush cs) === True
