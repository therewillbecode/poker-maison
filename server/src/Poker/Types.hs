{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Poker.Types where

import Control.Category ()
import Control.Lens 
import Data.Aeson (FromJSON, ToJSON)
import Data.Function (on)
import Data.Machine
import Data.Machine.Mealy
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Database.Persist.TH (derivePersistField)
import GHC.Base (NonEmpty)
import GHC.Generics (Generic)
import System.Random (Random)

data Rank
   = Two
   | Three
   | Four
   | Five
   | Six
   | Seven
   | Eight
   | Nine
   | Ten
   | Jack
   | Queen
   | King
   | Ace
   deriving (Eq, Read, Ord, Bounded, Enum, Generic, ToJSON, FromJSON)

instance Show Rank where
  show x =
    case x of
       Two -> "2"
       Three -> "3"
       Four -> "4"
       Five -> "5"
       Six -> "6"
       Seven -> "7"
       Eight -> "8"
       Nine -> "9"
       Ten -> "T"
       Jack -> "J"
       Queen -> "Q"
       King -> "K"
       Ace -> "A"

data Suit
   = Clubs
   | Diamonds
   | Hearts
   | Spades
   deriving (Eq, Ord, Bounded, Enum, Read, Generic, ToJSON, FromJSON)

instance Show Suit where
   show x = case x of
     Clubs -> "♧ "
     Diamonds -> "♢ "
     Hearts -> "♡ "
     Spades -> "♤ "

data Card = Card
   { rank :: Rank,
   suit :: Suit
   }
   deriving (Eq, Read, Generic, ToJSON, FromJSON)

instance Ord Card where
   compare = compare `on` rank

instance Show Card where
   show (Card r s) = show r ++ show s

data HandRank
 = HighCard
 | Pair
 | TwoPair
 | Trips
 | Straight
 | Flush
 | FullHouse
 | Quads
 | StraightFlush
 deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

type Bet = Int

--data ActivePlayerState
-- = SatOut -- SatOut denotes a player that will not be dealt cards unless they send a postblinds action to the server
-- | Folded
-- | In
-- deriving (Eq, Show, Ord, Enum, Bounded, Read, Generic, ToJSON, FromJSON)

data PocketCards
 = PocketCards Card Card
 deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

unPocketCards :: PocketCards -> [Card]
unPocketCards (PocketCards c1 c2) = [c1, c2]

newtype Chips = Chips Int
 deriving newtype (Num, Random)
 deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)

instance Semigroup Chips where
 (<>) (Chips a) (Chips b) = Chips $ a + b

-- The amount of chips bet by the player this turn.
newtype CommittedChips = CommittedChips Int deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)

instance Semigroup CommittedChips where
 (<>) (CommittedChips a) (CommittedChips b) = CommittedChips $ a + b

mkChips :: Int -> Maybe Chips
mkChips n
 | n < 0 = Nothing
 | otherwise = pure $ Chips n

unChips :: Chips -> Int
unChips (Chips n) = n

fromCommittedChips :: CommittedChips -> Int
fromCommittedChips (CommittedChips cs) = cs


newPlayer :: Text -> Int -> Player
newPlayer name chips =
 InHandP $ InHandPlayer
 { _playerName = name,
 _hasActed = NotActedThisTurn,
 _status = status,
 _pockets = Nothing,
 _chips = Chips chips,
 _currBet = Chips 0,
 _committed = CommittedChips 0,
 _possibleActions = []
 }
 where status = if chips == 0 then CannotAct IsAllIn else CanAct


data CanPlayerAct = PlayerCanAct | PlayerCannotAct
   deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

-- WasNotInLastHand:
-- Sometimes a player joins an in progress game and thus are
-- not on BB or SB position. PlayerWasNotInLastHand denotes
-- the fact the new player can choose to post an 'extra' blind
-- to play immediately. Or the player can wait till the blind
-- comes around to them.
--
newtype RequiredBlind = RequiredBlind (Maybe Blind)
   deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

data PlayedLastHand = HasNotPlayedLastHand | HasPlayedLastHand
   deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)

data HasPostedBlind = NotPostedBlind | PostedBlind Blind
   deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

data Blind
   = SmallBlind
   | BigBlind
   deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)


--data HasActedThisStreet = HasActed | HasNotActed
-- deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)

-- data GameStage = NotStarted | PostBlinds | HandUnderway Street

newtype PlayerPosition = PlayerPosition Int

data HandStatus
   = AwaitingPlayerAction
   | NotAwaitingPlayerAction
   | EveryoneFolded
   | EveryoneAllIn
   deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)

data Street
   = PreDeal
   | PreFlop
   | Flop
   | Turn
   | River
   | Showdown
   deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON)

-- Highest ranking hand for a given PlayerInfo that is in the game
-- during the Showdown stage of the game (last stage)
newtype PlayerShowdownHand
   = PlayerShowdownHand [Card]
   deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

unPlayerShowdownHand :: PlayerShowdownHand -> [Card]
unPlayerShowdownHand (PlayerShowdownHand cards) = cards

-- Folded To Signifies a a single player pot where everyone has
-- folded to them in this case the hand ranking is irrelevant
-- and the winner takes all. Therefore the winner has the choice of showing
-- or mucking (hiding) their cards as they are the only player in the pot.
--
-- Whereas in a MultiPlayer showdown all players must show their cards
-- as hand rankings are needed to ascertain the winner of the pot.
data Winners
 = MultiPlayerShowdown [((HandRank, PlayerShowdownHand), PlayerName)]
 | SinglePlayerShowdown PlayerName -- occurs when everyone folds to one player
 | NoWinners -- todo - remove this and wrap whole type in a Maybe
 deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

newtype Deck
 = Deck [Card]
 deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

unDeck :: Deck -> [Card]
unDeck (Deck cards) = cards

-- Idea - Could generalise the project to become
-- a DSL for card game servers.
-- (Game [Card]) [PlayerInfo] actions

-- With card games the rulechecking gets pretty nasty.
--
-- To tame the nastiness of validating game rules
-- we model the poker game as a products of mealy machines.
-- The table is a mealy machine and N players are N mealy machines.
-- So we have N machines for checking all players,
-- a few extra machines for checking the global rules
-- player is a mealy machine and game is a mealy machine?
-- like, this might be a good way to fight the complexity of building the whole state checker yourself
--
-- coding it ain't much hard either, but with having the composition abstracted out you kinda ensure that you don't forget about something when composing it manually
--
-- The key idea is
-- "small coherent parts of the ruleset to keep are different mealy machines"
--
-- We can then use property based testing to ensure invariants
-- hold between mealy machines. Also by modelling the game as a product
-- of machines it is easier to build game generators in PBT as we can
-- compose smaller generators which represent coherent rules of our game.

-- Machine a demand driven input source like a Pipe or Conduit.
-- A Machine from unlike a Pipe or Conduit support multiple inputs.
-- A Machine is constructed from a Plan

data AnErr = Err1
 deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)



--data PreHandPlayer =
-- --SatOutP SatOutPlayer
-- PreHandPlayer
-- | PreHandPlayer
-- | PreHandPlayer
-- deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)

{-

initInHandPlayer :: PlayerName -> Chips -> Mealy Action Player
initInHandPlayer name chips =
 unfoldMealy
 (\player action ->
 (nextInHandPlayer player action, next))
 newPlayer name chips
 
 

nextInHandPlayer :: InHandPlayer -> Action -> InHandPlayer
nextInHandPlayer (CanActP (CanActPlayer {..})) _ = undefined
nextInHandPlayer s _ = s


--each seat at table is a mealy machine
basically need a Player Monad which is essentially state monad (MealyT)
and then _playerState = 
 InHand (CanAct | CannotAct (Folded | AllIn))
 | PreHand (BlindRequired NoBlindRequired HasPostedBlind )
So yes scrap all the types you made and just encode them in a field
called playerState

player is a state monad with things like bet fold newStreet newHand

bet :: InHandPlayer -> Chips -> (Player, MealyT (Player) Action PlayerState)
bet (CanActP CanActPlayer{..}) bet'
 
 | _chips - bet' == 0 = 
 (InHandP $ CannotActP $ AllInP $ AllInPlayer{..}, 
 const $ Mealy $ InHandP $ CannotActP $ AllInP $ AllInPlayer{..})
 
 | otherwise = 
 (InHandP $ CanActP CanActPlayer{_chips = _chips - bet', ..}, canAct)
 
canAct :: Action -> ()
canAct = \case
 Fold -> (InHandP $ FoldedP $ FoldedPlayer {..}, Mealy folded)

folded :: Mealy Action PlayerState 
folded = (Mealy newHand

newHand :: MealyAction PlayerState
newHand 

-- board is a moore
placeBet :: Chips -> (InHandPlayer, Mealy Action InHandPlayer)
placeBet chips = (p, Mealy turn)

-- | Transition function from state M1A
check :: (GameStage, Mealy Action InHandPlayer)
check = (Turn' undefined, Mealy river)



InHand $ canAct 
InHand $ CannotAct Folded | AllIn

PreHand PostedBlindIfReqd
PreHand $ BlindRequired blind

SatOut

-}

--m2TransitionFmA :: BetAction -> (, Mealy Char M1State)
--m2TransitionFmA _ = (M1B,Mealy m2TransitionFmB)
--
--m2TransitionFmB :: FoldAction -> (M1State, Mealy Char M1State)
--m2TransitionFmB _ = (M1A,Mealy m2TransitionFmA)

--data CannotAct = AlreadyAllIn | HasFolded 
 -- deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)


hidePockets :: Player -> Player
hidePockets (InHandP (InHandPlayer{..})) =
 InHandP $ InHandPlayer{_pockets = Nothing, ..}
hidePockets p@(PreHandP _) = p
hidePockets p@(SatOutP _) = p




-- FoldedPlayer{..}
--foldPlayer (InHandP (CannotActP (AllInP AllInPlayer{..}))) =
-- FoldedPlayer{..}
--foldPlayer ( InHandP (CannotActP (FoldedP p))) = p
--
--foldPlayer (PreHandP ( PreHandPlayer{..})) =
-- FoldedPlayer{..}
--foldPlayer (PreHandP ( p)) = FoldedPlayer{..}
--foldPlayer (SatOutP (SatOutPlayer{..})) =
-- FoldedPlayer{..}


sitPlayerOut :: Player -> SatOutPlayer
sitPlayerOut (SatOutP p) = p
sitPlayerOut (InHandP InHandPlayer{..})= SatOutPlayer{..}
sitPlayerOut (PreHandP PreHandPlayer{..}) = SatOutPlayer{..}

data SatOutPlayer = SatOutPlayer {
    _playerName :: Text,
    _chips :: Chips
   } deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)

data BlindStatus = BlindRequired Blind | NoBlindRequired
   deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)

toBlindStatus :: Maybe Blind -> BlindStatus
toBlindStatus (Just b) = BlindRequired b 
toBlindStatus Nothing = NoBlindRequired

toPreHandPlayer :: Player -> Maybe Blind -> PreHandPlayer
toPreHandPlayer (PreHandP p) _ = p
toPreHandPlayer (InHandP InHandPlayer{..}) mbBlind =
 PreHandPlayer{_blindStatus = toBlindStatus mbBlind, ..}
toPreHandPlayer (SatOutP SatOutPlayer{..}) mbBlind =
 PreHandPlayer{_blindStatus = toBlindStatus mbBlind, ..}

data PreHandPlayer = PreHandPlayer {
    _playerName :: Text,
    _chips :: Chips,
    _committed ::CommittedChips,
    _blindStatus :: BlindStatus
   } deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)


--each seat at table is a mealy machine
--basically need a Player Monad which is essentially state monad (MealyT)
--and then 
 
data AllInOrFolded = IsAllIn | HasFolded
  deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)



data PlayerStatus = CanAct | CannotAct AllInOrFolded
  deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)

data InHandPlayer = InHandPlayer
   { _playerName :: Text,
   _hasActed :: HasActedThisStreet,
   _pockets :: Maybe PocketCards,
   _chips :: Chips,
   _currBet :: Chips,
   _status :: PlayerStatus,
   _committed :: CommittedChips,
   _possibleActions :: [Action]
   } deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)

data HasActedThisStreet = NotActedThisTurn | ActedThisTurn BetOrChecked 
   deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)


--data PlayerInHandStatus
-- = NotActedYet
-- | CanAct BetOrChecked
-- | Folded
-- | AllIn
-- deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)

data HasBet = HasCalled | HasBet Chips | HasRaised Chips
   deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)

--betSize :: HasBet -> Int
--betSize = \case
-- HasCalled -> n
-- HasBet n -> n
-- HasRaised n -> n

data BetOrChecked = MadeBet HasBet | Checked
   deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)


data Player =
    SatOutP SatOutPlayer
    | PreHandP PreHandPlayer
    | InHandP InHandPlayer
    deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)

--data InHandPlayer = 
-- CanActP CanActPlayer
-- | CannotActP CannotActPlayer 
-- deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)




--satIn :: Player -> Bool
--satIn (SatIn _) = True
--satIn _ = False


--satOut :: Player -> Bool
--satOut (SatOut _) = True
--satOut _ = False


-- | Plan to build a PlayerInfo Machine
-- validateMovePlan :: Plan (Either AnErr) PlayerMove (Either AnErr ())
-- validateMovePlan = do
-- -- awaits :: k i -> Plan k o i
-- -- check is valisd
-- a <- awaits $ Right Fold
-- yield
-- return 1

-- player is a mealyT or mealy

-- validateInGameMove is a pure machine (plan)

-- validatePreGameMove (sitIn vs sitout etc is a pure machine ) plan

-- validateBlinds move (canpostblind is a pure machine ) plan




------------------------- Game Stage -----------------------------------
newtype FlopBoard = FlopCards (Card, Card, Card)

newtype TurnBoard = TurnBoard (Card, Card, Card, Card)

newtype RiverBoard = RiverBoard (Card, Card, Card, Card, Card)

-- Moore machine since next state doesn't depend on input.
-- Note, you don't have a game until both small and big blinds are posted.
data GameStage
   = PreFlop'
   | Flop' FlopBoard
   | Turn' TurnBoard
   | River' RiverBoard
   | Showdown' Winners


data BettingStatus
   = AwaitingAction' PlayerName
   | NotAwaitingPlayerAction'
   | EveryoneFolded'
   | EveryoneAllIn'
   deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)
-------

-- If you can check, that is you aren't facing an amount you have to call,
-- then when you put in chips it is called a bet. If you have to put in
-- some amount of chips to continue with the hand, and you want to
-- increase the pot, it's called a raise. If it is confusing, just remember
-- this old poker adage: "You can't raise yourself."
--
-- Mucking hands refers to a player choosing not to
-- show his hands after everyone has folded to them. Essentially in
-- this scenario mucking or showing refers to the decision to
-- show ones hand or not to the table after everyone else has folded.
data Action'
   = SitDown' Player -- doesnt progress the game
   | LeaveSeat'' -- doesnt progress the game
   | PostBlind' Blind
   | Fold'
   | Call'
   | Raise' Chips
   | Check'
   | Bet' Chips
   | ShowHand'
   | MuckHand'
   | SitOut'
   | SitIn'
   | Timeout' -- REMOVE not an action
   deriving (Show, Ord, Eq, Read, Generic, ToJSON, FromJSON)

---------------------------------------------------------------

data GameSettings = GameSettings { 
   _minBuyInChips :: Chips,
   _maxBuyInChips :: Chips,
   _maxPlayers :: Int,
   _smallBlind :: Int,
   _bigBlind :: Int
  } deriving (Eq, Read, Show,Ord, Generic, ToJSON, FromJSON)

data GameNotRunning = GameNotRunning {
    _satOutPlayers :: [SatOutPlayer],
    _gameSettings :: GameSettings,
    _waitlist :: [PlayerName]
  } deriving (Eq, Read,Show, Ord, Generic, ToJSON, FromJSON)

data BlindsInProgress = BlindsInProgress {
  _players :: [PreHandPlayer],
  _satOutPlayers :: [SatOutPlayer],
  _gameSettings :: GameSettings,
  _waitlist :: [PlayerName],
  _dealer :: Int,
  _currentPosToAct :: Int 
  } deriving (Eq, Read,Show, Ord, Generic, ToJSON, FromJSON)

data HandInProgress = HandInProgress
 { _players :: [InHandPlayer],
   _satOutPlayers :: [SatOutPlayer],
   _gameSettings :: GameSettings,
   _board :: [Card],
   _waitlist :: [PlayerName],
   _deck :: Deck,
   _street :: Street,
   _pot :: Chips,
   _maxBet :: Chips,
   _dealer :: Int,
   _currentPosToAct :: Int -- If Nothing and not PreDeal stage of game then this signifies that
   -- no player can act (i.e everyone all in) or
   -- if during PreDeal (blinds stage) any player can act first in order to get the game started
   -- TODO refactor this logic into ADT such as Nobody | Anyone | Someone PlayerName PlayerPos
   }
   deriving (Eq, Read, Ord, Generic, ToJSON, FromJSON)

data HandFinished = HandFinished {
   _players :: [InHandPlayer],
   _satOutPlayers :: [SatOutPlayer],
   _winners :: Winners,
   _pot :: Chips,
   _dealer :: Int,
   _gameSettings :: GameSettings,
   _board :: [Card]
  } deriving (Eq, Show, Read, Ord, Generic, ToJSON, FromJSON)


instance Show HandInProgress where
   show HandInProgress {..} =
        "\n dealer: "
        <> show _dealer
           <> "\n _satOutPlayers: "
        <> show _satOutPlayers
        <> "\n _currentPosToAct: "
        <> show _currentPosToAct
        <> "\n _pot: "
        <> show _pot
        <> "\n _maxBet: "
        <> show _maxBet
        <> "\n _street: "
        <> show _street
        <> "\n _board: "
        <> show _board
        <> "\n _players: "
        <> show _players
  
type PlayerName = Text

data PlayerAction = PlayerAction
   { name :: PlayerName,
    action :: Action
   }
   deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

-- If you can check, that is you aren't facing an amount you have to call,
-- then when you put in chips it is called a bet. If you have to put in
-- some amount of chips to continue with the hand, and you want to
-- increase the pot, it's called a raise. If it is confusing, just remember
-- this old poker adage: "You can't raise yourself."
--
-- Mucking hands refers to a player choosing not to
-- show his hands after everyone has folded to them. Essentially in
-- this scenario mucking or showing refers to the decision to
-- show ones hand or not to the table after everyone else has folded.
data Action
   = SitDown Player -- doesnt progress the game
   | LeaveSeat' -- doesnt progress the game
   | PostBlind Blind
   | Fold
   | Call
   | Raise Chips
   | Check
   | Bet Chips
   | ShowHand
   | MuckHand
   | SitOut
   | SitIn
   | Timeout
   deriving (Show, Ord, Eq, Read, Generic, ToJSON, FromJSON)

data GameErr
   = NotEnoughChips PlayerName
   | OverMaxChipsBuyIn PlayerName
   | PlayerNotAtTable PlayerName
   | AlreadySatAtTable PlayerName
   | NotAtTable PlayerName
   | CannotSitAtFullTable PlayerName
   | AlreadyOnWaitlist PlayerName
   | InvalidMove
   PlayerName
   InvalidMoveErr
   deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)
  
-- ToDO -- ONLY ONE ERR MSG FOR EACH POSSIBLE ACTION
--
-- additional text field for more detailed info
--
-- i.e cannotBet "Cannot Bet Should Raise Instead - bets can only be made if there have been zero bets this street"
data InvalidMoveErr
   = BlindNotRequired
   | BlindRequiredErr Blind
   | NoBlindRequiredErr
   | BlindAlreadyPosted Blind
   | OutOfTurn CurrentPlayerToActErr -- _currentPosToAct is Just but not the player's index
   | NoPlayerCanAct -- _currentPosToAct is Nothing
   | CannotPostBlindOutsidePreDeal
   | CannotPostNoBlind -- if player tries to apply postBlind with a value of NoBlind
   | CannotPostBlind Text
   | InvalidActionForStreet
   | BetLessThanBigBlind
   | NotEnoughChipsForAction
   | CannotBetShouldRaiseInstead Text
   | PlayerToActNotAtTable
   | CannotRaiseShouldBetInstead
   | RaiseAmountBelowMinRaise Int
   | CannotCheckShouldCallRaiseOrFold
   | CannotCallZeroAmountCheckOrBetInstead
   | CannotShowHandOrMuckHand Text
   | CannotLeaveSeatOutsidePreDeal
   | CannotSitDownOutsidePreDeal
   | CannotSitInOutsidePreDeal
   | AlreadySatIn
   | AlreadySatOut -- cannot sitout when already satout
   | CannotSitOutOutsidePreDeal
   deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

newtype CurrentPlayerToActErr
 = CurrentPlayerToActErr PlayerName
 deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)



makeFieldsNoPrefix ''SatOutPlayer
makeFieldsNoPrefix ''PreHandPlayer
makeFieldsNoPrefix ''InHandPlayer

makeFieldsNoPrefix ''GameNotRunning
makeFieldsNoPrefix ''BlindsInProgress
makeFieldsNoPrefix ''HandInProgress
makeFieldsNoPrefix ''HandFinished


makeLenses ''Player
makeLenses ''GameSettings
makeLenses ''PlayerAction
makeLenses ''Winners

-- Due to the GHC Stage Restriction, the call to the Template Haskell function derivePersistField must be
-- in a separate module than where the generated code is used.
-- Perform marshaling using the Show and Read
-- instances of the datatype to string field in db
derivePersistField "Player"
derivePersistField "Winners"
derivePersistField "HandRank"
derivePersistField "Street"
derivePersistField "Card"