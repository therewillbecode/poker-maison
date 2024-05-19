{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Socket.Table where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    readTVar,
    readTVarIO,
    swapTVar,
    throwSTM,
  )
import Control.Lens ((^.))
import Control.Monad
  ( Monad (return),
    forM_,
    forever,
    mapM_,
    unless,
    void,
    when,
  )
import Control.Monad.Except
  ( Monad (return),
    MonadIO (liftIO),
    forM_,
    forever,
    mapM_,
    when,
  )
import Control.Monad.Reader
  ( Monad (return),
    MonadIO (liftIO),
    forM_,
    forever,
    mapM_,
    when,
  )
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.UTF8 (fromString)
import Data.Either (Either (Left, Right), isRight)
import qualified Data.Map.Lazy as M
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Text as T (Text, pack)
import Database (dbGetTableEntity, dbInsertGame)
import Database.Persist (Entity (Entity), PersistEntity (Key))
import Database.Persist.Postgresql
  ( ConnectionString,
    SqlPersistT,
    runMigration,
    withPostgresqlConn,
  )
import qualified Network.WebSockets as WS
import Pipes hiding (next)
import Pipes.Concurrent
  ( Input,
    Output,
    STM,
    atomically,
    fromInput,
    readTVar,
    toOutput,
  )
import Pipes.Core (push)
import Pipes.Parse (yield)
import qualified Pipes.Prelude as P
import Poker.ActionValidation (validateAction)
import Poker.Game.Game (countPlayersNotAllIn, doesPlayerHaveToAct, initPlayer)
import Poker.Game.Privacy (excludeOtherPlayerCards)
import Poker.Game.Utils
  ( getActivePlayers,
    getGamePlayer,
    getGamePlayerNames,
    shuffledDeck,
  )
import Poker.Poker
  ( canProgressGame,
  nextStage,
    progressGame,
    runPlayerAction,
  )
import Poker.Types
  ( Action (Bet, Call, Check, Fold, PostBlind, Raise, SitDown, Timeout),
    Blind (Big, Small),
    Game (..),
    Player (..),
    PlayerAction (PlayerAction, action, name),
    PlayerName,
    Street (PreDeal, Showdown),
    chips,
  )
import Schema (Key, TableEntity)
import Socket.Types
  ( Client (..),
    Lobby (..),
    MsgOut (NewGameState),
    ServerState (..),
    Table (..),
    TableConfig (..),
    TableDoesNotExistInLobby (TableDoesNotExistInLobby),
    TableName,
  )
import Socket.Utils (unLobby)
import Types ()
import System.Random
import Prelude
import Control.Monad.Error (MonadIO(liftIO))

setUpTablePipes ::
  ConnectionString -> TVar ServerState -> TableName -> Table -> IO (Async ())
setUpTablePipes connStr s name Table {..} = do
  t <- dbGetTableEntity connStr name
  let (Entity key _) = fromMaybe notFoundErr t
  async $
    forever $
      runEffect $
        (gamePipeline
          connStr
          s
          key
          name
          gameOutMailbox
          gameInMailbox)
  where
    notFoundErr = error $ "Table " <> show name <> " doesn't exist in DB"



runBot ::
  Output Game -> -- bots progress game with action and then push new game state here
  Text ->
  Pipe Game Game IO ()
runBot gameInMailbox
  botName = do
    g <- await
    validActions <- liftIO $ getValidBotActions g botName
    if null validActions then return () else takeAction g validActions
    yield g

    where 
      takeAction g validActions' = do 
         liftIO $ threadDelay (1 * 1000000) -- delay so can see whats going on

         randIx <- liftIO $ randomRIO (0, length validActions' - 1)
         let action' = validActions' !! randIx
         liftIO $ act' botName g action'

      act' :: Text -> Game -> Action -> IO ()
      act' botName  g a = do
        let botAction = PlayerAction {name = botName, action = a}
        let eitherNewGame = runPlayerAction g botAction
        case eitherNewGame of
          Left gameErr -> do 
            print "ERROR:  running a supposedly valid game action didn't work, not really a bot valid action"
            print  gameErr
            return ()
          Right newGame -> runEffect $ yield newGame >-> toOutput gameInMailbox


getValidBotActions :: Game -> PlayerName -> IO [Action]
getValidBotActions g@Game {..} name = do
  betAmount' <- randomRIO (lowerBetBound, chipCount)
  let possibleActions = actions _street betAmount'
      actionsValidated = validateAction g name <$> possibleActions
      pNameActionPairs = zip possibleActions actionsValidated
  return $ (<$>) fst $ filter (isRight . snd) pNameActionPairs
  where
    actions :: Street -> Int -> [Action]
    actions st chips
      | st == PreDeal = [PostBlind Big, PostBlind Small, SitDown (initPlayer name 1500)]
      | otherwise = [Check, Call, Fold, Bet chips, Raise chips]
    lowerBetBound = if _maxBet > 0 then 2 * _maxBet else _bigBlind
    chipCount = maybe 0 (^. chips) (getGamePlayer g name)

-- this is the pipeline of effects we run everytime a new game state
-- is placed in the tables
-- incoming mailbox for new game states.
--
-- New game states are send to the table's incoming mailbox every time a player acts
-- in a way that follows the game rules
--
-- Delays with "pause" at the end of each game stage (Flop, River etc) for UX
-- are done client side.
gamePipeline ::
  ConnectionString ->
  TVar ServerState ->
  Key TableEntity ->
  TableName ->
  Input Game ->
  Output Game ->
  Effect IO ()
gamePipeline connStr s key tableName outMailbox inMailbox = do
  fromInput outMailbox -- game actions go in this input sink from websocket connections
    >-> broadcast s tableName
    >-> logGame tableName
    >-> updateTable s tableName
    >-> writeGameToDB connStr key
    >-> nextStagePause
    >-> timePlayer s tableName
    >-> runBot inMailbox "bot1"
    >-> progress inMailbox -- new gamestates go in this output source


-- Delay to enhance UX based on game stages
timePlayer :: TVar ServerState -> TableName -> Pipe Game Game IO ()
timePlayer s tableName = do
  g@Game {..} <- await
  let currPlyrToAct = (!!) (getGamePlayerNames g) <$> _currentPosToAct
  liftIO $ forM_ currPlyrToAct $ runPlayerTimer s tableName g
  yield g

-- We watch incoming game states. We compare the initial gamestates
-- with the game state when the timer ends.
-- If the state is still the same then we timeout the player to act
-- to force the progression of the game.
runPlayerTimer ::
  TVar ServerState -> TableName -> Game -> PlayerName -> IO (Async ())
runPlayerTimer s tableName gameWhenTimerStarts plyrName = async $ do
  threadDelay (3 * 10000000) -- 30 seconds
  mbTable <- atomically $ getTable s tableName
  case mbTable of
    Nothing -> return ()
    Just Table {..} -> do
      let gameHasNotProgressed = gameWhenTimerStarts == game
          playerStillHasToAct = doesPlayerHaveToAct plyrName game
      when (gameHasNotProgressed && playerStillHasToAct) $
        case runPlayerAction game timeoutAction of
          Left err -> print err
          Right progressedGame ->
            runEffect $ yield progressedGame >-> toOutput gameInMailbox
  where
    timeoutAction = PlayerAction {name = plyrName, action = Timeout}

-- Delay to enhance UX so game doesn't move through stages
-- instantly when no players can act i.e everyone all in.
nextStagePause :: Pipe Game Game IO ()
nextStagePause = do
  g <- await
  when (canProgressGame g) $ liftIO $ threadDelay $ pauseDuration g
  yield g
  where
    pauseDuration :: Game -> Int
    pauseDuration g@Game {..}
      | _street == PreDeal = 0
      | _street == Showdown =
        1 * 1000000
      | -- 4 seconds
        countPlayersNotAllIn g <= 1 =
        1 * 1000000 -- everyone all in
      | otherwise = 1 * 1000000 -- 1 seconds

-- Progresses to the next state which awaits a player action.
--
--- If the next game state is one where no player action is possible
--  then we need to recursively progress the game.

--  These such states are:
--
--  1. everyone is all in.
--  1. All but one player has folded or the game.
--  3. Game is in the Showdown stage.
--
-- After each progression the new game state is sent to the table
-- mailbox. This sends the new game state through the pipeline that
-- the previous game state just went through.
progress :: Output Game -> Consumer Game IO ()
progress gameInMailbox = do
  g <- await
  when (canProgressGame g) (progress' g)
  -- <- liftIO getStdGen
  --liftIO $ print ("can progress the game to the next: " <> (show (progressGame gen g)))
  where
    progress' game = do
      gen <- liftIO getStdGen
      liftIO $ setStdGen $ snd $ next gen
      runEffect $ yield (progressGame gen game) >-> toOutput gameInMailbox

writeGameToDB :: ConnectionString -> Key TableEntity -> Pipe Game Game IO ()
writeGameToDB connStr tableKey = do
  g <- await
  _ <- liftIO $ async $ dbInsertGame connStr tableKey g
  yield g

-- write MsgOuts for new game states to outgoing mailbox for
-- client's who are observing the table
-- ensure they only get to see data they are allowed to see
informSubscriber :: TableName -> Game -> Client -> IO ()
informSubscriber n g Client {..} = do
  let filteredGame = excludeOtherPlayerCards clientUsername g
  runEffect $ yield (NewGameState n filteredGame) >-> toOutput outgoingMailbox
  return ()

-- sends new game states to subscribers
-- At the moment all clients receive updates from every game indiscriminately
broadcast :: TVar ServerState -> TableName -> Pipe Game Game IO ()
broadcast s n = do
  g <- await
  ServerState {..} <- liftIO $ readTVarIO s
  let usernames' = M.keys clients -- usernames to broadcast to
  liftIO $ async $ mapM_ (informSubscriber n g) clients
  return ()
  yield g


logGame :: TableName -> Pipe Game Game IO ()
logGame tableName = do
  g <- await
  gen <- newStdGen
  --liftIO $ print g
  --liftIO $ print $ "next stage: " <> show (nextStage gen g)
  yield g

-- Lookups up a table with the given name and writes the new game state
-- to the gameIn mailbox for propagation to observers.
--
-- If table with tableName is not found in the serverState lobby
-- then we just return () and do nothing.
toGameInMailbox :: TVar ServerState -> TableName -> Game -> IO ()
toGameInMailbox s name game = do
  table' <- atomically $ getTable s name
  forM_ table' send
  where
    send Table {..} = runEffect $ yield game >-> toOutput gameInMailbox

-- Get a combined outgoing mailbox for a group of clients who are observing a table
--
-- Here we monoidaly combined so we then have one mailbox
-- we use to broadcast new game states to which will be sent out to each client's
-- socket connection under the hood
combineOutMailboxes :: [Client] -> Consumer MsgOut IO ()
combineOutMailboxes clients = toOutput $ foldMap outgoingMailbox clients

getTable :: TVar ServerState -> TableName -> STM (Maybe Table)
getTable s tableName = do
  ServerState {..} <- readTVar s
  return $ M.lookup tableName $ unLobby lobby

updateTable :: TVar ServerState -> TableName -> Pipe Game Game IO ()
updateTable serverStateTVar tableName = do
  g <- await
  liftIO $ atomically $ updateTable' serverStateTVar tableName g
  yield g

updateTable' :: TVar ServerState -> TableName -> Game -> STM ()
updateTable' serverStateTVar tableName newGame = do
  ServerState {..} <- readTVar serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwSTM $ TableDoesNotExistInLobby tableName
    Just table@Table {..} -> do
      let updatedLobby = updateTableGame tableName newGame lobby
      swapTVar serverStateTVar ServerState {lobby = updatedLobby, ..}
      return ()

updateTableAndGetMailbox ::
  TVar ServerState -> TableName -> Game -> STM (Maybe (Output Game))
updateTableAndGetMailbox serverStateTVar tableName newGame = do
  ServerState {..} <- readTVar serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwSTM $ TableDoesNotExistInLobby tableName
    Just table@Table {..} -> do
      let updatedLobby = updateTableGame tableName newGame lobby
      swapTVar serverStateTVar ServerState {lobby = updatedLobby, ..}
      return $ Just gameInMailbox

updateTableGame :: TableName -> Game -> Lobby -> Lobby
updateTableGame tableName newGame (Lobby lobby) =
  Lobby $
    M.adjust updateTable tableName lobby
  where
    updateTable Table {..} = Table {game = newGame, ..}
