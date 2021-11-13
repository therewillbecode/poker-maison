{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Schema where

import Control.Monad ()
import Data.Aeson ()
import Data.Aeson.Types ()
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Poker.Types
  ( Bet,
    Card,
    PlayerInfo,
    PlayerName,
    Street,
    Winners,
  )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  UserEntity json sql=users
    username Text
    email Text
    password Text
    availableChips Int
    chipsInPlay Int
    createdAt UTCTime default=now()
    UniqueEmail email
    UniqueUsername username
    deriving Show Read
  TableEntity json sql=tables
    name Text
    UniqueName name
    deriving Show Read
  GameEntity json sql=games
    tableID TableEntityId
    createdAt UTCTime default=now()
    players [PlayerInfo]
    minBuyInChips Int
    maxBuyInChips Int
    maxPlayers Int
    board [Card]
    winners Winners
    waitlist [PlayerName]
    deck [Card]
    smallBlind Int
    bigBlind Int
    street Street
    pot Int
    maxBet Bet
    dealer Int
    currentPosToAct Int Maybe
    deriving Show Read
|]
