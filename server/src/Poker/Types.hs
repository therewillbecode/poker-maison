{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Poker.Types where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Database.Persist.TH (derivePersistField)
import GHC.Generics (Generic)

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
  show x = case x of
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

data PlayerState
  = SatOut -- SatOut denotes a player that will not be dealt cards unless they send a postblinds action to the server
  | Folded
  | In
  deriving (Eq, Show, Ord, Enum, Bounded, Read, Generic, ToJSON, FromJSON)

data Street
  = PreDeal
  | PreFlop
  | Flop
  | Turn
  | River
  | Showdown
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON)

data Player = Player
  { _pockets :: Maybe PocketCards,
    _chips :: Int,
    _bet :: Bet,
    _playerState :: PlayerState,
    _playerName :: Text,
    _possibleActions :: [Action],
    _committed :: Bet,
    _actedThisTurn :: Bool
  }
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

data PocketCards
  = PocketCards Card Card
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

unPocketCards :: PocketCards -> [Card]
unPocketCards (PocketCards c1 c2) = [c1, c2]

-- Highest ranking hand for a given Player that is in the game
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
  | NoWinners
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

newtype Deck
  = Deck [Card]
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

unDeck :: Deck -> [Card]
unDeck (Deck cards) = cards

data Game = Game
  { _players :: [Player],
    _minBuyInChips :: Int,
    _maxBuyInChips :: Int,
    _maxPlayers :: Int,
    _board :: [Card],
    _winners :: Winners,
    _waitlist :: [PlayerName],
    _deck :: Deck,
    _smallBlind :: Int,
    _bigBlind :: Int,
    _street :: Street,
    _pot :: Int,
    _maxBet :: Bet,
    _dealer :: Int,
    _currentPosToAct :: Maybe Int -- If Nothing and not PreDeal stage of game then this signifies that
    -- no  player can act (i.e everyone all in) or
    -- if during PreDeal (blinds stage) any player can act first in order to get the game started
  }
  deriving (Eq, Read, Ord, Generic, ToJSON, FromJSON)

instance Show Game where
  show Game {..} =
    "\n dealer: "
      <> show _dealer
      <> "\n _currentPosToAct: "
      <> show _currentPosToAct
      <> "\n _smallBlind: "
      <> show _smallBlind
      <> "\n _big_blind: "
      <> show _bigBlind
      <> "\n _minBuyin: "
      <> show _minBuyInChips
      <> "\n _maxBuyin: "
      <> show _maxBuyInChips
      <> "\n _pot: "
      <> show _pot
      <> "\n _maxBet: "
      <> show _maxBet
      <> "\n _street: "
      <> show _street
      <> "\n _winners: "
      <> show _winners
      <> "\n _board: "
      <> show _board
      <> "\n _players: "
      <> show _players

type PlayerName = Text

data Blind
  = Small
  | Big
  | NoBlind
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

data PlayerAction = PlayerAction
  { name :: PlayerName,
    action :: Action
  }
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

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
  | Raise Int
  | Check
  | Bet Int
  | ShowHand
  | MuckHand
  | SitOut
  | SitIn
  | Timeout
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

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

--
-- additional text field for more detailed info
--
-- i.e cannotBet "Cannot Bet Should Raise Instead - bets can only be made if there have been zero bets this street"
data InvalidMoveErr
  = BlindNotRequired
  | BlindRequired Blind
  | NoBlindRequired
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

makeLenses ''Player

makeLenses ''PlayerAction

makeLenses ''Game

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
