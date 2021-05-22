{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poker.HandSpec where

import Control.Lens ()
import Control.Monad ()
import Data.Aeson ()
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either ()
import Data.List ()
import Data.List.Lens ()
import Data.List.Split ()
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace ()
import GHC.Generics ()
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Poker.ActionValidation ()
import Poker.Game.Blinds ()
import Poker.Game.Game ()
import Poker.Game.Hands (maybeFlush, value)
import Poker.Game.Utils ()
import Poker.Generators (genSameSuitCards, genShuffledCards)
import Poker.Poker ()
import Poker.Types ()
import System.IO.Unsafe ()
import Test.Hspec (describe, it)

prop_same_num_cards_after_valuation :: Property
prop_same_num_cards_after_valuation = property $ do
  sevenCards <- forAll (genShuffledCards 7)
  let (_, cs) = value sevenCards
  length cs === 5

prop_7suited_cards_always_a_flush :: Property
prop_7suited_cards_always_a_flush = property $ do
  cs <- forAll $ genSameSuitCards 7
  isJust (maybeFlush cs) === True

spec = do
  describe "value" $ do
    it "Number of cards before and after valuation is involutive" $ do
      require prop_same_num_cards_after_valuation
  it "7 suited cards always a flush" $ require prop_7suited_cards_always_a_flush
