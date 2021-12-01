{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poker.FSMSpec where

import Control.Lens
  ( Field2 (_2),
    FunctorWithIndex (imap),
    (%~),
    (.~),
    (^.),
    element
  )
import Control.Monad
import Control.Monad.State
import Data.Either ()
import qualified Data.List as List
import Data.Maybe (Maybe (..))
import Data.Proxy ()
import Data.Text (Text)
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Traversable (mapAccumR)
import Data.Tuple (fst, swap)
import qualified Data.Vector as V
import System.Random (getStdGen)
import Debug.Trace ()
import GHC.Enum (Enum (fromEnum))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Poker.Poker
import qualified Hedgehog.Range as Range
import Test.Hspec (describe, it)
import Test.Hspec.Hedgehog
import qualified Hedgehog.Range as Range
import Poker.Game.Game
import Poker.Game.Utils (shuffledDeck)
import Poker.Game.Utils
  ( getActivePlayers,
    getPlayersSatIn,
    initialDeck,
  )
import Poker.Types
import Data.IORef
import Prelude
import Hedgehog

initialModel :: GameModel v
initialModel = GModel GNotStarted [] (MaxPlayerCount 6) (MaxBuyInChips 3000) (MinBuyInChips 1500)


data GBlindsStatus =  
    GBlindPosting
  | GBlindPostingFinished
  deriving (Eq, Ord, Show)

data GHandStatus = 
     GStreetFinished
   | GPlayerNeedsToAct Int
   | GEveryoneFolded
   | GEveryoneAllIn
    deriving (Eq, Ord, Show)

data GStatus = GNotStarted | GBlinds GBlindsStatus | GHandInProgress GHandStatus
  deriving (Eq, Ord, Show)



data PInBlindStatus = PHasPosted Blind | PHasNotPostedBlind deriving (Eq, Ord, Show)

data GPlayer = PInBlind PInBlindStatus | PInHand PInHandStatus | PSatOut 
  deriving (Eq, Ord, Show)

data PInHandStatus = PFolded | PNotFolded
  deriving (Eq, Ord, Show)

newtype MinBuyInChips = MinBuyInChips Int deriving (Eq, Ord, Show)

newtype MaxBuyInChips = MaxBuyInChips Int deriving (Eq, Ord, Show)

newtype MaxPlayerCount = MaxPlayerCount Int deriving (Eq, Ord, Show)

------------------
-- Player actions
------------------

data GNewPlayer =
    GNewPlayer Text Int
  deriving (Eq, Show)

newtype PSitDown (v :: * -> *) =
    PSitDown GNewPlayer
  deriving (Eq, Show)

instance HTraversable PSitDown where
  htraverse _ (PSitDown (GNewPlayer n c)) = pure (PSitDown (GNewPlayer n c))

data GProgressGame (v :: * -> *) =
    GProgressGame
  deriving (Eq, Show)

newtype PPostBlind (v :: * -> *) =
    PPostBlind Int
  deriving (Eq, Show)

newtype PBet (v :: * -> *) =
    PBet Int
  deriving (Eq, Show)

newtype PCall (v :: * -> *) =
    PCall Int
  deriving (Eq, Show)

newtype PCheck (v :: * -> *) =
    PCheck Int
  deriving (Eq, Show)

newtype PFold (v :: * -> *) =
    PFold Int
  deriving (Eq, Show)

---------------------------

data GameModel (v :: * -> *) =
    GModel GStatus [GPlayer] MaxPlayerCount MaxBuyInChips MinBuyInChips
  deriving (Eq, Ord, Show)




genNewPlayer :: Int -> Int -> Int -> Gen GNewPlayer
genNewPlayer pos minChips maxChips = do 
    cs <- Gen.int $ Range.constant minChips maxChips
    return $ GNewPlayer (T.pack $ show pos) cs

newGameIO :: IO (IORef Game)
newGameIO = do
    randGen <- getStdGen
    newIORef $ initialGameState $ shuffledDeck randGen

s_pos_big_blind :: (MonadTest m, MonadIO m) => IORef Game -> Command (GenT Identity) m GameModel
s_pos_big_blind ref = undefined


s_sit_down_new_player :: (MonadTest m, MonadIO m) => IORef Game -> Command (GenT Identity) m GameModel
s_sit_down_new_player ref =
  let
    -- This generator only produces an action to sit down when the game hand has not started yet.
    gen state =
      case state of
        (GModel (GNotStarted) ps (MaxPlayerCount i)  (MaxBuyInChips maxChips) (MinBuyInChips minChips)  ) ->
            if length ps < i
                then 
                Just $ fmap PSitDown $ genNewPlayer (length ps) minChips maxChips
                else Nothing
        _ -> Nothing
 
    execute :: (MonadTest m, MonadIO m) => PSitDown v -> m Game
    execute (PSitDown (GNewPlayer name chips))  = do
       prevGame <- liftIO $ readIORef ref
       newGame <- evalEither $ runPlayerAction prevGame playerAction
       liftIO $ atomicWriteIORef ref newGame
       return newGame
      where playerAction = PlayerAction { name = name, action = SitDown $ initPlayer name chips}

  in
    Command gen execute [
        -- Precondition: the 
        Require $ 
          \(GModel gStatus ps maxPs (MaxBuyInChips maxChips)
           (MinBuyInChips minChips))
           (PSitDown (GNewPlayer name chips)) -> 
               gStatus == GNotStarted
                && chips >= minChips && chips <= maxChips

        -- Update: add player to table in model
      , Update $ \(GModel gStatus ps maxPs maxChips minChips) (PSitDown _) (game :: Var Game v) ->
          let newPlayer = PInBlind PHasNotPostedBlind
          in (GModel gStatus (ps <> pure newPlayer ) maxPs maxChips minChips)

        -- Postcondition: player added to table
      , Ensure $ \(GModel gStatus prevPlayers _ _ _) (GModel _ nextPlayers _ _ _) (PSitDown _) _ -> do
          length nextPlayers === (length prevPlayers) + 1
          gStatus === GNotStarted
      ]


spec = 
    describe "fsm" $ do

      it "Status" $
        hedgehog $ do
                ref <- liftIO newGameIO
                actions <- forAll $
                   Gen.sequential (Range.linear 1 6) initialModel [
                       s_sit_down_new_player ref
                   
                     ]
                executeSequential initialModel actions
