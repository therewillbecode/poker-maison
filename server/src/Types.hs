{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.Redis (ConnectInfo)
import GHC.Generics (Generic)
import Servant ()
import Servant.API
  ( Capture,
    Get,
    JSON,
    (:>),
  )
import Servant.Auth.Server (FromJWT, ToJWT)

type RedisConfig = ConnectInfo

type Password = Text

data Login = Login
  { loginUsername :: Text,
    loginPassword :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Register = Register
  { newUserEmail :: Text,
    newUsername :: Username,
    newUserPassword :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Username
  = Username Text
  deriving (Generic, Show, Read, Eq, Ord, ToJWT, FromJWT)

unUsername :: Username -> Text
unUsername (Username username) = username

instance ToJSON Username

instance FromJSON Username

type UserID = Text

data UserProfile = UserProfile
  { proUsername :: Username,
    proEmail :: Text,
    proAvailableChips :: Int,
    proChipsInPlay :: Int,
    proUserCreatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ReturnToken = ReturnToken
  { access_token :: Text,
    refresh_token :: Text,
    expiration :: Int --seconds to expire
  }
  deriving (Generic, ToJSON, FromJSON)
