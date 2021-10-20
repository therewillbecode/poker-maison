{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Types where

import Control.Concurrent (MVar)
import Control.Concurrent.STM (TChan, TVar)
import Control.Concurrent.STM.TChan (TChan)
import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import Database.Persist.Postgresql (ConnectionString)
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import Pipes.Concurrent (Input, Output)
import Pipes
  ( Consumer,
    Effect,
    Pipe,
    await,
    runEffect,
    yield,
    (>->),
  )
import Poker.Types
  ( Action,
    Game,
    GameErr,
  )
import Types
  ( RedisConfig,
    Username,
  )

data MsgHandlerConfig = MsgHandlerConfig
  { dbConn :: ConnectionString,
    serverStateTVar :: TVar ServerState,
    username :: Username,
    clientConn :: WS.Connection,
    redisConfig :: RedisConfig
  }

type TableName = Text

newtype Lobby
  = Lobby (Map TableName Table)
  deriving (Ord, Eq, Show)

instance Show ServerState where
  show _ = ""

-- exception when adding subscriber to table if subscriber already exists inside STM transaction
newtype CannotAddAlreadySubscribed
  = CannotAddAlreadySubscribed Text
  deriving (Show)

instance Exception CannotAddAlreadySubscribed

-- exception for cannot find a table with given TableName in Lobby inside STM transaction
newtype TableDoesNotExistInLobby
  = TableDoesNotExistInLobby Text
  deriving (Show)

instance Exception TableDoesNotExistInLobby

data TableConfig = TableConfig
  { botCount :: Int, -- number of bots to maintain at table
    minHumans :: Int -- number of humans to wait for before starting a game
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

headsUpBotsConfig :: TableConfig
headsUpBotsConfig =
  TableConfig
    { botCount = 2,
      minHumans = 0
    }

data Table = Table
  { subscribers :: [Username], -- observing public game state includes players sat down
    gameOutMailbox :: Input Game, -- outgoing MsgOuts broadcasts -> write source for msgs to propagate new game states to clients
    gameInMailbox :: Output Game, --incoming gamestates -> read (consume) source for new game states
    waitlist :: [Username], -- waiting to join a full table
    bots :: Maybe (Consumer Game IO ()),
    game :: Game,
    channel :: TChan MsgOut,
    config :: TableConfig
  }

instance Show Table where
  show Table {..} =
    show subscribers <> "\n" <> show waitlist <> "\n" <> show game

instance Eq Table where
  Table {game = game1} == Table {game = game2} = game1 == game2

instance Ord Table where
  Table {game = game1} `compare` Table {game = game2} =
    game1 `compare` game2

data Client = Client
  { clientUsername :: Text,
    conn :: WS.Connection,
    outgoingMailbox :: Output MsgOut
  }

instance Show Client where
  show Client {..} = show clientUsername

data ServerState = ServerState
  { clients :: Map Username Client,
    lobby :: Lobby
  }

instance Eq Client where
  Client {clientUsername = clientUsername1} == Client {clientUsername = clientUsername2} =
    clientUsername1 == clientUsername2

-- incoming messages from a ws client
data MsgIn
  = GetTables
  | SubscribeToTable TableName
  | LeaveTable
  | GameMsgIn GameMsgIn
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GameMsgIn
  = TakeSeat
      TableName
      Int
  | LeaveSeat TableName
  | GameMove TableName Action
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- For the lobby view so client can make an informed decision about which game to join
data TableSummary = TableSummary
  { _tableName :: Text,
    _playerCount :: Int,
    _minBuyInChips :: Int,
    _maxBuyInChips :: Int,
    _maxPlayers :: Int,
    _waitlistCount :: Int,
    _smallBlind :: Int,
    _bigBlind :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- outgoing messages for clients
data MsgOut
  = TableList [TableSummary]
  | SuccessfullySatDown
      TableName
      Game
  | SuccessfullyLeftSeat TableName
  | SuccessfullySubscribedToTable
      TableName
      Game
  | GameMsgOut GameMsgOut
  | NewGameState TableName Game
  | ErrMsg Err
  | AuthSuccess
  | Noop
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GameMsgOut
  = GameMoveErr Err
  | PlayerLeft
  | PlayerJoined TableName Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Err
  = TableFull TableName
  | TableDoesNotExist TableName
  | NotSatAtTable TableName
  | AlreadySatInGame TableName
  | NotSatInGame TableName
  | AlreadySatAtTable TableName
  | AlreadySubscribedToTable TableName
  | NotEnoughChipsToSit
  | GameErr GameErr
  | InvalidGameAction
  | ChipAmountNotWithinBuyInRange TableName
  | UserDoesNotExistInDB Text
  | AuthFailed Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype Token = Token Text -- JWT
