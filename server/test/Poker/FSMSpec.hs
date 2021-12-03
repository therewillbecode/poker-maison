{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poker.FSMSpec where

import Control.Lens
import Control.Monad
import Text.Pretty.Simple
import Control.Monad.State
import Data.Either ()
import qualified Data.Text.Lazy as L
import qualified Data.List as List
import Data.Maybe (Maybe (..))
import Data.Proxy ()
import Data.Text (Text)
import Data.List
import Data.List.Lens
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Traversable (mapAccumR)
import Data.Tuple (fst, swap)
import qualified Data.Vector as V
import System.Random (getStdGen)
import Debug.Trace 
import GHC.Enum (Enum (fromEnum))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Poker.Poker
import qualified Hedgehog.Range as Range
import Test.Hspec (describe, it) 
import Test.Hspec.Hedgehog hiding (Action)
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
import Hedgehog hiding (Action)


data GPlayers (v :: * -> *) = NotInited' | GPlayers (Var (Vector [Action]) v) deriving (Eq, Ord, Show)

data GToAct (v :: * -> *) = NotInited | Inited (Var (Maybe Int) v)  deriving (Eq, Ord, Show)

data GModel (v :: * -> *) = GModel (GPlayers v) (GToAct v) deriving (Eq, Ord, Show)


initialModel :: forall  (v :: * -> *) . GModel v
initialModel = GModel NotInited'  NotInited

--data GHandStatus = 
--     GStreetFinished
--   | GPlayerNeedsToAct Int
--   | GEveryoneFolded
--   | GEveryoneAllIn
--    deriving (Eq, Ord, Show)



genNewPlayer :: Int -> Gen GNewPlayer
genNewPlayer pos = do 
    cs <- Gen.int $ Range.constant 1500 2000
    return $ GNewPlayer (T.pack $ show pos) cs


newGameIO :: IO (IORef Game)
newGameIO = do
    randGen <- getStdGen
    newIORef $ initialGameState $ shuffledDeck randGen

resetGameIO :: IORef Game -> IO ()
resetGameIO ref = do
    randGen <- getStdGen
    atomicWriteIORef ref $ initialGameState $ shuffledDeck randGen





data GNewPlayer = GNewPlayer Text Int
  deriving (Eq, Show)

newtype PSitDown (v :: * -> *) =
    PSitDown GNewPlayer
  deriving (Eq, Show)

instance HTraversable PSitDown where
  htraverse _ (PSitDown (GNewPlayer n c)) = 
      pure (PSitDown (GNewPlayer n c))


s_sit_down_new_player :: (MonadTest m, MonadIO m) => IORef Game -> Command (GenT Identity) m GModel
s_sit_down_new_player ref =
  let
    -- This generator only produces an action to sit down when the game hand has not started yet.
    gen state =
      case state of
        (GModel (GPlayers (Var ps)) toAct) ->
            if V.length ps < 5
                then Just $ fmap PSitDown $ genNewPlayer (V.length ps + 1)
                else Nothing
        _ -> Nothing
 
    execute :: (MonadTest m, MonadIO m) => PSitDown v -> m ( Vector [Action])
    execute (PSitDown (GNewPlayer name chips))  = do
       prevGame <- liftIO $ readIORef ref
       footnote $ "Action: New player \""  <> T.unpack name <>  "\" sat down"
       footnote $ L.unpack $ pShowDarkBg prevGame
       footnote  "\n"
       footnote  "\n"
       newGame <- evalEither $ runPlayerAction prevGame playerAction
       liftIO $ atomicWriteIORef ref newGame
       return $ V.fromList $ getAllValidPlayerActions  newGame
   --    return $ (_currentPosToAct newGame, getAllValidPlayerActions newGame)
      where
           playerAction = PlayerAction { name = name, action = SitDown $ initPlayer name chips}

  in
    Command gen execute [
        -- Precondition: the 
        Require $ 
          \(GModel (GPlayers ps) toAct)
           (PSitDown (GNewPlayer name chips)) ->
                (V.length ps) < 5

        -- Update: add player to table in model
      , Update $ \(GModel (GPlayers ps) toAct) (PSitDown _)  (actions :: Var (Vector [Action]) v) ->
          (GModel   (GPlayers actions) NotInited)

        -- Postcondition: player added to table
      , Ensure $ \(GModel (GPlayers prevPlayers) toAct) (GModel (GPlayers nextPlayers) nextToAct) 
                  (PSitDown _) _ -> do
          V.length nextPlayers === (V.length prevPlayers) + 1          
      ]


spec = 
    describe "fsm" $ do

      it "Status" $
        hedgehog $ do
                ref <- liftIO newGameIO
                actions <- forAll $
                   Gen.sequential (Range.linear 1 3) initialModel [
                       s_sit_down_new_player ref
                ---       s_act ref
                 --      s_time_out_post_blind ref
                   
                     ]
                liftIO $ resetGameIO ref
                executeSequential initialModel actions


{-

------------------
-- Player actions
------------------

data GNewPlayer =
    GNewPlayer Text Int
  deriving (Eq, Show)

instance HTraversable PTimeout where
  htraverse f (PTimeout pos) = PTimeout <$> htraverse f pos

data PTimeout (v :: * -> *) =
    PTimeout (Var PlayerPos v)
  deriving (Eq, Show)

newtype PSitDown (v :: * -> *) =
    PSitDown GNewPlayer
  deriving (Eq, Show)

instance HTraversable PSitDown where
  htraverse _ (PSitDown (GNewPlayer n c)) = pure (PSitDown (GNewPlayer n c))

data GProgressGame (v :: * -> *) =
    GProgressGame
  deriving (Eq, Show)



data PPostBlind (v :: * -> *) =
    PPostBlind (Var (PlayerPos, GBlind) v)
  deriving (Eq, Show)

instance HTraversable PPostBlind where
  htraverse f (PPostBlind a) = PPostBlind <$> htraverse f a

data PBet (v :: * -> *) =
    PBet PlayerPos Int
  deriving (Eq, Show)

data PCall (v :: * -> *) =
    PCall PlayerPos Int
  deriving (Eq, Show)

data PCheck (v :: * -> *) =
    PCheck PlayerPos Int
  deriving (Eq, Show)

data PFold (v :: * -> *) =
    PFold PlayerPos Int
  deriving (Eq, Show)

---------------------------

--data GameModel (v :: * -> *) =
--    GModel (GStatus v) ( [GPlayer] ) Dealer MaxPlayerCount MaxBuyInChips MinBuyInChips
--

genNewPlayer :: Int -> Int -> Int -> Gen GNewPlayer
genNewPlayer pos minChips maxChips = do 
    cs <- Gen.int $ Range.constant minChips maxChips
    name <- Gen.int $ Range.constant 0 10000000
    return $ GNewPlayer (T.pack $ show name) cs


newGameIO :: IO (IORef Game)
newGameIO = do
    randGen <- getStdGen
    newIORef $ initialGameState $ shuffledDeck randGen

resetGameIO :: IORef Game -> IO ()
resetGameIO ref = do
    randGen <- getStdGen
    atomicWriteIORef ref $ initialGameState $ shuffledDeck randGen


dropSatOutPs :: [GPlayer] -> [GPlayer]
dropSatOutPs = filter (/= PSatOut)


nextElemfromNth :: (a -> Bool) -> [a] -> Int -> Maybe a
nextElemfromNth f ps n = find f $ drop n $ cycle ps 


-- TODO Should also add actions / commands that should fail i.e cannot bet when all in etc
-- you just flip the either for this - see example in yow lambda talk.

--NEED to use VARS see
--https://stackoverflow.com/questions/56406210/haskell-define-specialization-of-function-thats-polymorphic-in-type

--reqBlinds :: Dealer ->  [GPlayer] -> Maybe [(PlayerPos, GBlind)]
--reqBlinds (Dealer dlr) ps 
--  | length actives < 2 = Just []
--  | length actives == 2 = 
--             case dealerPlusNActives dlr actives 1 of 
--               Just (_, bbPos) -> Just [(PlayerPos dlr, SB), (bbPos, BB)]
--               Nothing         -> Nothing
--
--  | otherwise = case [dealerPlusNActives dlr actives 1, 
--                      dealerPlusNActives dlr actives 2] of
--                  [Just (_, sbPos), Just (_, bbPos)] -> Just [(sbPos, SB), (bbPos, BB)]
--                  _                        -> Nothing
--   where actives = filter ((/= PSatOut) . fst) $ zip ps $ PlayerPos <$> [0..]
--
--dealerPlusNActives dealerPos actives n = nextElemfromNth (const True) (cycle actives) (dealerPos + n)
--
--
--nextBlind :: Var PlayerPos v -> Dealer -> [GPlayer] -> Maybe (Var (PlayerPos, GBlind) v)
--nextBlind (Var postPos) dlr ps = do
--    reqBlinds' <- (reqBlinds dlr ps)
--    posBlind <- nextElemfromNth ((/=) postPos . fst) reqBlinds' 1
--    return $ Var posBlind
--    --where
--    --  ixPosted :: Maybe Int
--    --  ixPosted = (findIndex ((/=) postPos . fst)) pos
--    --  remainingBlinds :: Maybe [(PlayerPos, GBlind)]
--    --  remainingBlinds = (filter ((/=) postPos . fst)) <$> (reqBlinds dlr ps)
--

data GBlind = BB | SB
  deriving (Eq, Ord, Show)


data GAwaitingBlind (v :: * -> *) = GAwaitingBlind (Var (PlayerPos, GBlind) v)
  deriving (Eq, Ord, Show)

data GBlindsStatus (v :: * -> *) =  
    GBlindPosting (GAwaitingBlind v)
  | GBlindPostingFinished
  deriving (Eq, Ord, Show)
  
data PBlindStatus = PHasPostedBlind | PHasNotPostedBlind deriving (Eq, Ord, Show)

genPostBlind :: [(PlayerPos, GBlind)] -> Gen (PlayerPos, GBlind)
genPostBlind bs = do
  (a :: (PlayerPos, GBlind) ) <- Gen.element bs
  return a

s_post_blind :: (MonadTest m, MonadIO m) => IORef Game -> Command Gen m GameModel
s_post_blind ref =
  let 
    gen state = do
        case state of
          -- Another player already posted a blind to start the blind action
          -- Pick the next requireed blind
          (GModel (GBlinds (GBlindPosting (GAwaitingBlind a))) ps _  _ _ _ ) -> 
              pure $ pure $ PPostBlind a
             -- b@(Var (pos :: PlayerPos , blind' :: GBlind))))) 
             --   ps _  _ _ _) ->
             -- case ps ^? ix pos of
             --     Nothing -> error "awaiting pos to act out of bounds"
             --     Just (PInBlind (PHasPostedBlind)) -> error $ (show pos) <> "pos already posted"
             --     Just (PInBlind (PHasNotPostedBlind)) ->  
             --        --let 
             --        --  g :: Gen (PPostBlind v)
             --        --  g =  pure $ PPostBlind $ (PlayerPos pos, blind)
             --        -- in
             --            (Just $ pure b ::  Maybe (Gen (PPostBlind v)))
            
          -- Pick any possible blind
          (GModel (GNotStarted) ps dlr  _ _ _) ->
            let
               blindGen :: Maybe [(PlayerPos, GBlind)]
               blindGen = reqBlinds dlr ps
            in case blindGen of
                Nothing -> Nothing
                Just [] -> Nothing
                Just (bs :: [(PlayerPos, GBlind)] ) ->
                    if null bs
                      then Nothing
                      else (Just $ aa bs  :: Maybe (Gen (PPostBlind v)))
  
          _ -> Nothing
        where 
          aa :: [(PlayerPos, GBlind)] -> Gen (PPostBlind v)
          aa as = do
              as <- Gen.element as
              undefined
              --return $ PPostBlind $ Var as
            
    execute :: (MonadTest m, MonadIO m) => PPostBlind v -> m Game
    execute (PPostBlind (Var (PlayerPos pos, blind))) = do
       prevGame <- liftIO $ readIORef ref
       footnote $ "Action from position " <> show pos <> ": Posted " <> show blind
       footnote $ L.unpack $ pShowDarkBg prevGame
       footnote  "\n"
       footnote  "\n"
       let pName = ((prevGame ^. players) !! pos ) ^. playerName
       annotateShow blind
       newGame <- evalEither 
                     $ runPlayerAction prevGame 
                     $ PlayerAction { name = pName, action = PostBlind $ blind' blind  }
       liftIO $ atomicWriteIORef ref newGame
       return newGame
      where
        blind' BB = Big
        blind' SB = Small

  in
    Command gen execute [
            -- Precondition: the 
        Require $ 
          \(GModel gStatus ps maxPs _ _ _) ( PPostBlind (Var ((PlayerPos pos), blind)))  -> 
            canPostBlindAtStage ps gStatus && (blindsPostedCount ps < 2)

        -- Update: add blinds status in model
      ,
       let 
          markPostedBlind :: [GPlayer] -> Var (PlayerPos, GBlind) v -> [GPlayer]
          markPostedBlind ps (Var (PlayerPos i, _)) = ps & ix i .~ PInBlind PHasPostedBlind

          nextStatus :: Maybe (Var (PlayerPos, GBlind) v) -> GStatus v
          nextStatus = GBlinds . (maybe GBlindPostingFinished (GBlindPosting . GAwaitingBlind))

       in
          Update $ \(GModel gStatus ps dlr maxPs maxChips minChips)
            (PPostBlind (varPosBlind :: (Var (PlayerPos, GBlind) v)))  (newGame :: Var Game v) ->
          
             let newPs = markPostedBlind ps (varPosBlind)
                 mbNextBlind =  --trace (show pos <> "next " <> (show $ nextBlind (PlayerPos pos) dlr ps)) 
                                (nextBlind (fst <$> varPosBlind) dlr newPs) 
                 newGStatus =  (nextStatus mbNextBlind)
             in (GModel newGStatus newPs dlr maxPs maxChips minChips)
--
         -- Postcondition: 
       , Ensure $ \(GModel _ prevPlayers _ _ _ _) (GModel gStatus nextPlayers _ _ _ _) 
         (PPostBlind (Var (pos, _))) (game :: Game) -> 
         do
         --  diff
           annotateShow (awaitingBlind gStatus)
           annotateShow pos
         --  diff (Just $ PInBlind PHasNotPostedBlind) (==) 
         --          (case (awaitingBlind gStatus) of
         --             Nothing -> Nothing
         --             Just (Var (PlayerPos awaitPos)) ->
         --                 nextPlayers ^? ix awaitPos)


--           assert $ awaitingBlindFromPlyrNotPosted newPs $ awaitingBlind gStatus
           diff (awaitingBlind gStatus) (/=) (Just pos)
           assert $ blindsPostedCount nextPlayers <= 2
           diff (blindsPostedCount prevPlayers) (<) (blindsPostedCount nextPlayers)
           game ^. street === PreDeal
           length prevPlayers === length nextPlayers
      ]
  where    
    canPostBlindAtStage :: [GPlayer] -> GStatus v -> Bool
    canPostBlindAtStage _ (GBlinds (GBlindPosting _)) = True
    canPostBlindAtStage ps GNotStarted = length (dropSatOutPs ps) > 1 
    canPostBlindAtStage _ _ = False

--    awaitingBlindFromPlyrNotPosted :: [GPlayer] -> Maybe PlayerPos -> Maybe
--    awaitingBlindFromPlyrNotPosted ps (Just pos) = case ps ^? pos of 
--        Nothing -> error "awaiting blind from pos ix out of range" 
--        Just (PInBlind PHasPostedBlind ) -> False
--        Just (PInBlind PHasNotPostedBlind ) -> True
--        Just (PSatOut PHasPostedBlind) -> error "awaiting blind from sat out player"
--
--    awaitingBlindFromPlyrNotPosted _ _ True  

blindsPostedCount :: [GPlayer] -> Int
blindsPostedCount = length . filter ((==) (PInBlind PHasPostedBlind))

--s_time_out_post_blind :: (MonadTest m, MonadIO m) => IORef Game -> Command (GenT Identity) m GameModel
--s_time_out_post_blind ref =
-- let
--    -- Fake a timeout occuring when a player has to post a blind.
--    gen state =
--      case state of
--          (GModel gStatus _ _ _ _ _) -> 
--            case (awaitingBlind gStatus) of 
--               Nothing -> Nothing
--               Just (Var (pos, _)) -> Just $ pure $ PTimeout $ Var pos 
--
--    execute :: (MonadTest m, MonadIO m) => PTimeout v -> m Game
--    execute (PTimeout (Var (PlayerPos pos)  )) = do
--       prevGame <- liftIO $ readIORef ref
--       footnote $ "Action from position " <> show pos <> ": Timeout "
--       footnote $ L.unpack $ pShowDarkBg prevGame
--       footnote  "\n"
--       footnote  "\n"
--       let pName = ((prevGame ^. players) !! pos ) ^. playerName
--       newGame <- evalEither 
--                     $ runPlayerAction prevGame 
--                     $ PlayerAction { name = pName, action = Timeout }
--       liftIO $ atomicWriteIORef ref newGame
--       return newGame
-- in
--    Command gen execute [
--            -- Precondition: the 
--        Require $ 
--          \(GModel gStatus ps maxPs _ _ _)  (PTimeout posVar) ->
--              case gStatus of
--                 GBlinds (GBlindPosting (GAwaitingBlind (Var posBlind))) -> 
--                    (Var $ fst <$> posBlind) == posVar  
--                 _ -> False
--
--      ,
--       let 
--          markSatOut :: [GPlayer] -> Var PlayerPos v -> [GPlayer]
--          markSatOut ps (Var ((PlayerPos i))) = ps & ix i .~ PSatOut
--
--          nextStatus :: Maybe (Var (PlayerPos, GBlind) v) -> GStatus v
--          nextStatus = GBlinds . (maybe GBlindPostingFinished (GBlindPosting . GAwaitingBlind))
--
--       in
--          Update $ \(GModel gStatus ps dlr maxPs maxChips minChips) 
--                    (PTimeout posVar)
--                    (newGame :: Var Game v) ->
--             let newPs = markSatOut ps posVar
--                 mbNextBlind = nextBlind posVar dlr ps
--                 blindsPosted = length $ filter ((==) $ PInBlind PHasPostedBlind) ps
--                 notPostedBlind = length $ filter ((==)  $ PInBlind PHasNotPostedBlind) ps
--                 newGStatus = if (blindsPosted < 2) && (notPostedBlind > 0) 
--                                 then nextStatus mbNextBlind
--                                 else GNotStarted
--             in (GModel newGStatus newPs dlr maxPs maxChips minChips)
----
--         -- Postcondition: 
--       , Ensure $ \(GModel _ prevPlayers _ _ _ _) 
--                   (GModel gStatus nextPlayers _ _ _ _)
--                   (PTimeout pos) 
--                   (game :: Game) -> 
--         do 
--           --diff (awaitingBlind gStatus) (/=) (Just  (Var (pos, GBlind) _))
--           assert $ blindsPostedCount nextPlayers <= 2
--           game ^. street === PreDeal
--           length prevPlayers === length nextPlayers
--      ]
--

awaitingBlind :: forall (v :: * -> *).  GStatus v -> Maybe (Var (PlayerPos, GBlind) v)
awaitingBlind  (GBlinds (GBlindPosting (GAwaitingBlind a))) = Just a
awaitingBlind _ = Nothing


s_sit_down_new_player :: (MonadTest m, MonadIO m) => IORef Game -> Command (GenT Identity) m GameModel
s_sit_down_new_player ref =
  let
    -- This generator only produces an action to sit down when the game hand has not started yet.
    gen state =
      case state of
        (GModel (GNotStarted) ps _ (MaxPlayerCount maxPlayers) (MaxBuyInChips maxChips) (MinBuyInChips minChips)) ->
            if length ps < maxPlayers
                then Just $ fmap PSitDown $ genNewPlayer (length ps + 1) minChips maxChips
                else Nothing
        _ -> Nothing
 
    execute :: (MonadTest m, MonadIO m) => PSitDown v -> m Game
    execute (PSitDown (GNewPlayer name chips))  = do
       prevGame <- liftIO $ readIORef ref
       footnote $ "Action: New player \""  <> T.unpack name <>  "\" sat down"
       footnote $ L.unpack $ pShowDarkBg prevGame
       footnote  "\n"
       footnote  "\n"
       newGame <- evalEither $ runPlayerAction prevGame playerAction
       liftIO $ atomicWriteIORef ref newGame
       return newGame
      where playerAction = PlayerAction { name = name, action = SitDown $ initPlayer name chips}

  in
    Command gen execute [
        -- Precondition: the 
        Require $ 
          \(GModel gStatus ps _ (MaxPlayerCount maxPs) (MaxBuyInChips maxChips)
           (MinBuyInChips minChips))
           (PSitDown (GNewPlayer name chips)) ->
               gStatus == GNotStarted
                 && chips >= minChips && chips <= maxChips
                  && length ps < maxPs
                    && read (T.unpack name) > length ps
                      && (length ps) < maxPs

        -- Update: add player to table in model
      , Update $ \(GModel gStatus ps maxPs dlr maxChips minChips) (PSitDown _) (game :: Var Game v) ->
          let newPlayer = PInBlind PHasNotPostedBlind
          in (GModel gStatus (ps <> pure newPlayer ) maxPs dlr maxChips minChips)

        -- Postcondition: player added to table
      , Ensure $ \(GModel gStatus prevPlayers _ _ _ _) (GModel _ nextPlayers _ _ _ _) (PSitDown _) _ -> do
          length nextPlayers === (length prevPlayers) + 1
          gStatus === GNotStarted
          
      ]


spec = 
    describe "fsm" $ do

      it "Status" $
        hedgehog $ do
                ref <- liftIO newGameIO
                actions <- forAll $
                   Gen.sequential (Range.linear 1 9) initialModel [
                       s_sit_down_new_player ref,
                       s_post_blind ref
                 --      s_time_out_post_blind ref
                   
                     ]
                liftIO $ resetGameIO ref
                executeSequential initialModel actions

-}