{-# LANGUAGE RecordWildCards #-}

module Socket.Workers where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM
  ( TChan,
    TVar,
    atomically,
    dupTChan,
    readTChan,
  )
import Control.Concurrent.STM.TChan (TChan, dupTChan, readTChan)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import Database
  ( dbGetTableEntity,
    dbInsertTableEntity,
    dbRefillAvailableChips,
  )
import Database.Persist (Entity (Entity), PersistEntity (Key))
import Database.Persist.Postgresql
  ( ConnectionString,
    SqlPersistT,
    runMigration,
    withPostgresqlConn,
  )
import Schema (Key, TableEntity)
import Socket.Types
  ( Lobby (..),
    MsgOut (NewGameState),
    ServerState,
    Table
      ( Table,
        channel,
        game,
        gameInMailbox,
        gameOutMailbox,
        subscribers,
        waitlist
      ),
    TableName,
  )

forkBackgroundJobs ::
  ConnectionString -> TVar ServerState -> Lobby -> IO [Async ()]
forkBackgroundJobs connString serverStateTVar lobby = do
  forkChipRefillDBWriter connString chipRefillInterval chipRefillThreshold -- Periodically refill player chip balances when too low.
  forkGameDBWriters connString lobby -- At the end of game write new game and player data to the DB.
  where
    chipRefillInterval = 50000000 -- 1 mins
    chipRefillThreshold = 200000 -- any lower chip count will be topped up on refill to this amount

-- Fork a new thread for each table that writes game updates received from the table channel to the DB
forkGameDBWriters :: ConnectionString -> Lobby -> IO [Async ()]
forkGameDBWriters connString (Lobby lobby) =
  sequence $
    ( \(tableName, Table {..}) -> forkGameDBWriter connString channel tableName
    )
      <$> M.toList lobby

-- Looks up the tableName in the DB to get the key and if no corresponsing  table is found in the db then
-- we insert a new table to the db. This step is necessary as we use the TableID as a foreign key in the
-- For Game Entities in the DB.
-- After we have the TableID we fork a new process which listens to the channel which emits new game states
-- for a given table. For each new game state msg received we write the new game state into the DB.
forkGameDBWriter ::
  ConnectionString -> TChan MsgOut -> TableName -> IO (Async ())
forkGameDBWriter connString chan tableName = do
  maybeTableEntity <- dbGetTableEntity connString tableName
  case maybeTableEntity of
    Nothing -> do
      tableKey <- dbInsertTableEntity connString tableName
      forkGameWriter tableKey
    Just (Entity tableKey _) -> forkGameWriter tableKey
  where
    forkGameWriter tableKey =
      async (writeNewGameStatesToDB connString chan tableKey)

writeNewGameStatesToDB ::
  ConnectionString -> TChan MsgOut -> Key TableEntity -> IO ()
writeNewGameStatesToDB connString chan tableKey = do
  dupChan <- atomically $ dupTChan chan
  forever $ do
    chanMsg <- atomically $ readTChan dupChan
    case chanMsg of
      (NewGameState tableName game) -> return ()
      _ -> return ()

-- Fork a thread which refills low player chips balances in DB at a given interval
forkChipRefillDBWriter :: ConnectionString -> Int -> Int -> IO (Async ())
forkChipRefillDBWriter connString interval chipsThreshold =
  async $
    forever $ do
      dbRefillAvailableChips connString chipsThreshold
      threadDelay interval
