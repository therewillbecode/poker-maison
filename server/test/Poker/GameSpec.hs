{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poker.GameSpec where

import Control.Lens (element, (%~), (&), (.~), (?~), (^.))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog
  ( Property,
    assert,
    forAll,
    property,
    withDiscards,
    (===),
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Poker.Game.Game
  ( allButOneAllIn,
    allButOneFolded,
    awaitingPlayerAction,
    dealToPlayers,
    doesPlayerHaveToAct,
    everyoneAllIn,
    getHandRankings,
    getNextHand,
    haveAllPlayersActed,
    nextPosToAct,
    progressToFlop,
    progressToPreFlop,
    progressToRiver,
    progressToShowdown,
    progressToTurn,
  )
import Poker.Game.Utils (getActivePlayers, initialDeck)
import Poker.Generators
  ( allPStates,
    allPStreets,
    genGame,
    genPlayer',
    genPlayers,
  )
import Poker.Poker (initialGameState)
import Poker.Types
  ( Card (Card, rank, suit),
    Deck (Deck),
    Game (..),
    Player (..),
    PlayerState (Folded, In, SatOut),
    PocketCards (PocketCards),
    Rank (Four, King, Three),
    Street (Flop, PreDeal, PreFlop, River, Showdown, Turn),
    Suit (Clubs, Diamonds, Hearts, Spades),
    Winners (NoWinners),
    actedThisTurn,
    bet,
    chips,
    committed,
    currentPosToAct,
    dealer,
    deck,
    maxBet,
    playerState,
    players,
    pot,
    smallBlind,
    street,
  )
import Test.Hspec (describe, it, shouldBe)

initialGameState' :: Game
initialGameState' = initialGameState initialDeck

player1 :: Player
player1 =
  Player
    { _pockets =
        Just $
          PocketCards
            Card {rank = Three, suit = Diamonds}
            Card {rank = Four, suit = Spades},
      _chips = 2000,
      _bet = 50,
      _playerState = In,
      _playerName = "player1",
      _committed = 50,
      _actedThisTurn = True,
      _possibleActions = []
    }

player2 :: Player
player2 =
  Player
    { _pockets =
        Just $
          PocketCards
            Card {rank = Three, suit = Clubs}
            Card {rank = Four, suit = Hearts},
      _chips = 2000,
      _bet = 0,
      _playerState = In,
      _playerName = "player2",
      _committed = 50,
      _actedThisTurn = False,
      _possibleActions = []
    }

player3 :: Player
player3 =
  Player
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerState = In,
      _playerName = "player3",
      _committed = 50,
      _actedThisTurn = False,
      _possibleActions = []
    }

player4 :: Player
player4 =
  Player
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerState = SatOut,
      _playerName = "player4",
      _committed = 0,
      _actedThisTurn = False,
      _possibleActions = []
    }

player5 :: Player
player5 =
  Player
    { _pockets =
        Just $
          PocketCards
            Card {rank = King, suit = Diamonds}
            Card {rank = Four, suit = Spades},
      _chips = 2000,
      _bet = 50,
      _playerState = In,
      _playerName = "player1",
      _committed = 50,
      _actedThisTurn = True,
      _possibleActions = []
    }

player6 :: Player
player6 =
  Player
    { _pockets = Nothing,
      _chips = 2000,
      _bet = 0,
      _playerState = SatOut,
      _playerName = "player6",
      _committed = 0,
      _actedThisTurn = False,
      _possibleActions = []
    }

initPlayers :: [Player]
initPlayers = [player1, player2, player3]

turnGameThreePlyrs :: Game
turnGameThreePlyrs =
  Game
    { _dealer = 2,
      _currentPosToAct = Just 0,
      _smallBlind = 25,
      _bigBlind = 50,
      _minBuyInChips = 1500,
      _maxBuyInChips = 3000,
      _pot = 550,
      _maxBet = 0,
      _street = Turn,
      _winners = NoWinners,
      _board = [],
      _maxPlayers = 6,
      _waitlist = [],
      _deck = Deck [],
      _players =
        [ Player
            { _pockets = Nothing,
              _chips = 2197,
              _bet = 0,
              _playerState = In,
              _playerName = "player0",
              _committed = 50,
              _actedThisTurn = False,
              _possibleActions = []
            },
          Player
            { _pockets = Nothing,
              _chips = 1847,
              _bet = 0,
              _playerState = In,
              _playerName = "player1",
              _committed = 250,
              _actedThisTurn = False,
              _possibleActions = []
            },
          Player
            { _pockets = Nothing,
              _chips = 2072,
              _bet = 0,
              _playerState = In,
              _playerName = "player2",
              _committed = 250,
              _actedThisTurn = False,
              _possibleActions = []
            }
        ]
    }

prop_plyrShouldntActWhenNotInPos :: Property
prop_plyrShouldntActWhenNotInPos = withDiscards 225 . property $ do
  g <- forAll $ genGame allPStreets allPStates
  let g' = g & currentPosToAct ?~ 1
  doesPlayerHaveToAct "player0" g' === False

prop_plyrShouldntActWhenNoChips :: Property
prop_plyrShouldntActWhenNoChips = withDiscards 225 . property $ do
  g <- forAll $ genGame allPStreets allPStates
  let g' = g & players . element 0 %~ chips .~ 0
  doesPlayerHaveToAct "player0" g' === False

prop_plyrShouldntActDuringPreDealWhenNotEnoughPlyrsToStartGame :: Property
prop_plyrShouldntActDuringPreDealWhenNotEnoughPlyrsToStartGame = withDiscards 225 . property $ do
  g <- forAll (genGame [PreDeal] allPStates)
  (p, _) <- forAll $ genPlayer' PreDeal allPStates 0 []
  let g' = g & players .~ [p]
  doesPlayerHaveToAct "player0" g' === False

prop_number_of_hand_rankings_and_active_players_same :: Property
prop_number_of_hand_rankings_and_active_players_same = withDiscards 225 . property $ do
  plyrCount <- forAll $ Gen.int $ Range.linear 2 9
  (ps, cs) <- forAll $ genPlayers Showdown requiredActives allPStates plyrCount deck
  length (getHandRankings ps cs) === length (getActivePlayers ps)
  where
    requiredActives = 1
    Deck deck = initialDeck

prop_nextPosToActLTPlayerCount :: Property
prop_nextPosToActLTPlayerCount = withDiscards 225 . property $ do
  g <- forAll $ genGame allPStreets allPStates
  let pCount = length $ _players g
      nextPos = fromMaybe 0 (nextPosToAct g)
  assert $ nextPos < pCount

prop_when_everyone_allIn_next_pos_Nothing :: Property
prop_when_everyone_allIn_next_pos_Nothing = withDiscards 500 . property $ do
  g <- forAll $ Gen.filter everyoneAllIn (genGame allPStreets allPStates)
  nextPosToAct g === Nothing

prop_when_awaiting_action_nextPos_always_Just :: Property
prop_when_awaiting_action_nextPos_always_Just = withDiscards 600 . property $ do
  g <- forAll $ Gen.filter awaitingPlayerAction (genGame [PreFlop, Flop, Turn, River] [In, Folded])
  isNothing (nextPosToAct g) === False

spec = do
  describe "dealToPlayers" $
    it "should deal correct number of cards" $ do
      let (_, newPlayers) = dealToPlayers initialDeck [player1, player3]
      all
        ( \Player {..} ->
            if _playerState == In
              then isJust _pockets
              else isNothing _pockets
        )
        newPlayers
        `shouldBe` True

  describe "haveAllPlayersActed" $ do
    it
      "should return True when all players have acted during PreDeal for Three Players"
      $ do
        let game =
              (street .~ PreDeal) . (maxBet .~ 0)
                . ( players
                      .~ [ ( (playerState .~ In) . (actedThisTurn .~ False) . (bet .~ 0)
                               . (committed .~ 0)
                           )
                             player1,
                           ( (playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 0)
                               . (committed .~ 25)
                           )
                             player2,
                           ( (playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 0)
                               . (committed .~ 50)
                           )
                             player6
                         ]
                  )
                $ initialGameState'
        haveAllPlayersActed game `shouldBe` True

    it
      "should return False when not all players acted during PreDeal for Three Players"
      $ do
        let unfinishedBlindsGame =
              (street .~ PreDeal) . (players .~ [player1, player4, player6]) $
                initialGameState'
        haveAllPlayersActed unfinishedBlindsGame `shouldBe` False

    it
      "should return True when all players have acted during preFlop for Two Players"
      $ do
        let game =
              (street .~ PreFlop) . (maxBet .~ 0)
                . ( players
                      .~ [ ((playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 0))
                             player1,
                           ((playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 0))
                             player2
                         ]
                  )
                $ initialGameState'
        haveAllPlayersActed game `shouldBe` True

    it
      "should return False when not all players acted during PreFlop for Two Players"
      $ do
        let unfinishedBlindsGame =
              (street .~ PreDeal) . (players .~ [player1, player4]) $
                initialGameState'
        haveAllPlayersActed unfinishedBlindsGame `shouldBe` False

  describe "allButOneFolded" $ do
    it "should return True when all but one player " $ do
      let game =
            (street .~ PreFlop)
              . (players .~ [(playerState .~ Folded) player1, player2])
              $ initialGameState'
      allButOneFolded game `shouldBe` True

    it "should return False when not all players acted" $ do
      let unfinishedBlindsGame =
            (street .~ PreFlop) . (players .~ [player1, player3]) $
              initialGameState'
      allButOneFolded unfinishedBlindsGame `shouldBe` False

    it "should always return False for PreDeal (blinds) stage" $ do
      let unfinishedBlindsGame =
            (street .~ PreDeal)
              . (players .~ [(playerState .~ Folded) player1, player2])
              $ initialGameState'
      allButOneFolded unfinishedBlindsGame `shouldBe` False

  describe "progressToPreFlop" $ do
    let preDealGame =
          (street .~ PreDeal) . (maxBet .~ 50) . (pot .~ 75)
            . (deck .~ initialDeck)
            . ( players
                  .~ [ ((chips .~ 1000) . (committed .~ 25) . (bet .~ 25)) player5,
                       ((chips .~ 1000) . (committed .~ 50) . (bet .~ 50)) player2
                     ]
              )
            $ initialGameState'

    let preFlopGame = progressToPreFlop preDealGame

    it "should update street to PreFlop" $ preFlopGame ^. street `shouldBe` PreFlop

    it "should not reset any player bet" $ do
      let playerBets = (^. bet) <$> _players preFlopGame
      playerBets `shouldBe` [25, 50]

    it "Dealer position acts first during Preflop game stage when Heads Up (2 plyrs)" $ do
      let game =
            (street .~ PreDeal) . (currentPosToAct ?~ 0)
              . (players .~ [player1, player1])
              . (dealer .~ 0)
              $ initialGameState'
      _currentPosToAct preFlopGame `shouldBe` Just (_dealer game)

  describe "progressToFlop" $ do
    let preFlopGame =
          (street .~ Flop) . (maxBet .~ 1000) . (pot .~ 1000)
            . (deck .~ initialDeck)
            . (players .~ [(chips .~ 1000) player5, (chips .~ 1000) player2])
            $ initialGameState'
    let flopGame = progressToFlop preFlopGame

    it "should update street to Turn" $ flopGame ^. street `shouldBe` Flop

    it "should reset maxBet" $ flopGame ^. maxBet `shouldBe` 0

    it "should reset all player bets" $ do
      let playerBets = (^. bet) <$> _players flopGame
      playerBets `shouldBe` [0, 0]

  describe "progressToTurn" $ do
    let flopGame =
          (street .~ Flop) . (maxBet .~ 1000) . (pot .~ 1000)
            . (deck .~ initialDeck)
            . (players .~ [(chips .~ 1000) player5, (chips .~ 1000) player2])
            $ initialGameState'
    let turnGame = progressToTurn flopGame

    it "should update street to Turn" $ turnGame ^. street `shouldBe` Turn

    it "should reset maxBet" $ turnGame ^. maxBet `shouldBe` 0

    it "should reset all player bets" $ do
      let playerBets = (^. bet) <$> _players turnGame
      playerBets `shouldBe` [0, 0]

  describe "progressToRiver" $ do
    let turnGame =
          (street .~ Turn) . (maxBet .~ 1000) . (pot .~ 1000)
            . (deck .~ initialDeck)
            . (players .~ [(chips .~ 1000) player5, (chips .~ 1000) player2])
            $ initialGameState'
    let riverGame = progressToRiver turnGame

    it "should update street to River" $ riverGame ^. street `shouldBe` River

    it "should reset maxBet" $ riverGame ^. maxBet `shouldBe` 0

    it "should reset all player bets" $ do
      let turnGame =
            (street .~ Turn) . (maxBet .~ 1000) . (pot .~ 1000)
              . (deck .~ initialDeck)
              . (players .~ [(chips .~ 1000) player5, (chips .~ 1000) player2])
              $ initialGameState'
      let riverGame = progressToRiver turnGame
      let playerBets = (^. bet) <$> _players riverGame
      playerBets `shouldBe` [0, 0]

  describe "progressToShowdown" $ do
    let riverGame =
          (street .~ River) . (pot .~ 1000) . (deck .~ initialDeck)
            . (players .~ [(chips .~ 1000) player5, (chips .~ 1000) player2])
            $ initialGameState'
    let showdownGame = progressToShowdown riverGame

    it "should update street to Turn" $ showdownGame ^. street `shouldBe` Showdown

    it "should award pot chips to winner of hand" $ do
      let playerChipCounts = (^. chips) <$> _players showdownGame
      playerChipCounts `shouldBe` [2000, 1000]

    it "should split pot if more than one player wins given pot" $ do
      let riverGame =
            (street .~ River) . (pot .~ 1000) . (deck .~ initialDeck)
              . (players .~ [(chips .~ 1000) player1, (chips .~ 1000) player2])
              $ initialGameState'
      let showdownGame = progressToShowdown riverGame
      let playerChipCounts =
            (\Player {..} -> _chips) <$> _players showdownGame
      playerChipCounts `shouldBe` [1500, 1500]

  describe "getNextHand" $ do
    let showdownGame =
          (street .~ Showdown) . (maxBet .~ 1000) . (pot .~ 1000)
            . (deck .~ initialDeck)
            . (dealer .~ 1)
            . (players .~ [(chips .~ 1000) player5, (chips .~ 1000) player2])
            $ initialGameState'
    let preDealGame = getNextHand showdownGame $ Deck []

    it "should update street to PreDeal" $
      preDealGame ^. street
        `shouldBe` PreDeal

    it "should reset maxBet" $ preDealGame ^. maxBet `shouldBe` 0

    it "should reset all player bets" $ do
      let playerBets = (\Player {..} -> _bet) <$> _players preDealGame
      playerBets `shouldBe` [0, 0]

    it "should increment dealer position" $ preDealGame ^. dealer `shouldBe` 0

  describe "allButOneAllIn" $ do
    it "should return False for two player game if no one all in" $ do
      let preFlopGame' =
            (street .~ PreFlop) . (pot .~ 1000) . (deck .~ initialDeck)
              . ( players
                    .~ [ ((playerState .~ In) . (actedThisTurn .~ False)) player1,
                         ((playerState .~ In) . (actedThisTurn .~ True)) player3
                       ]
                )
              $ initialGameState'
      allButOneAllIn preFlopGame' `shouldBe` False

    it "should return True for two player game if one player is all in and other isn't" $ do
      let preFlopGame' =
            (street .~ PreFlop) . (currentPosToAct ?~ 0) . (pot .~ 10) . (deck .~ initialDeck)
              . ( players
                    .~ [ ((playerState .~ In) . (chips .~ 0) . (bet .~ 0) . (actedThisTurn .~ False)) player1,
                         ((playerState .~ In) . (chips .~ 1) . (bet .~ 0) . (actedThisTurn .~ False)) player3
                       ]
                )
              $ initialGameState'
      allButOneAllIn preFlopGame' `shouldBe` True

    it
      "should return True for two player game if a player has called the other player all in"
      $ do
        let preFlopGame =
              (street .~ PreFlop) . (maxBet .~ 1950) . (pot .~ 4000)
                . (deck .~ initialDeck)
                . ( players
                      .~ [ ( (actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 1950)
                               . (chips .~ 0)
                               . (committed .~ 2000)
                           )
                             player1,
                           ( (playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 1950)
                               . (committed .~ 2000)
                               . (chips .~ 3000)
                           )
                             player3
                         ]
                  )
                $ initialGameState'
        allButOneAllIn preFlopGame `shouldBe` True

    it
      "should return False for two player game if a player bet all in and the other has folded"
      $ do
        let preFlopGame =
              (street .~ PreFlop) . (maxBet .~ 1950) . (pot .~ 4000)
                . (deck .~ initialDeck)
                . ( players
                      .~ [ ( (actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 1950)
                               . (chips .~ 0)
                               . (committed .~ 2000)
                           )
                             player1,
                           ( (playerState .~ Folded) . (actedThisTurn .~ True)
                               . (bet .~ 1950)
                               . (committed .~ 2000)
                               . (chips .~ 3000)
                           )
                             player3
                         ]
                  )
                $ initialGameState'
        allButOneAllIn preFlopGame `shouldBe` False

    it
      "should return False for three player game if only one short stacked player all in"
      $ do
        let preFlopGame =
              (street .~ PreFlop) . (maxBet .~ 1950) . (pot .~ 1000)
                . (deck .~ initialDeck)
                . ( players
                      .~ [ ( (actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 1950)
                               . (chips .~ 0)
                               . (committed .~ 2000)
                           )
                             player1,
                           ( (playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 1950)
                               . (committed .~ 2000)
                               . (chips .~ 3000)
                           )
                             player3,
                           ( (playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 1950)
                               . (committed .~ 2000)
                               . (chips .~ 3000)
                           )
                             player3
                         ]
                  )
                $ initialGameState'
        allButOneAllIn preFlopGame `shouldBe` False

    it "should return True for four player game if only one player not all in" $ do
      let flopGame =
            (street .~ Flop) . (maxBet .~ 2000) . (pot .~ 10000)
              . (deck .~ initialDeck)
              . ( players
                    .~ [ ( (actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 0)
                             . (chips .~ 0)
                             . (committed .~ 2000)
                         )
                           player1,
                         ( (actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 2000)
                             . (committed .~ 4000)
                             . (chips .~ 0)
                         )
                           player3,
                         ( (playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 2000)
                             . (committed .~ 4000)
                             . (chips .~ 0)
                         )
                           player3,
                         ( (playerState .~ In) . (actedThisTurn .~ False) . (bet .~ 0)
                             . (committed .~ 2000)
                             . (chips .~ 800)
                         )
                           player3
                       ]
                )
              $ initialGameState'
      allButOneAllIn flopGame `shouldBe` True

  describe "everyoneAllIn" $
    it "should return True for three player game if everyone is all in" $ do
      let flopGame =
            (street .~ Flop) . (maxBet .~ 2000) . (pot .~ 10000)
              . (deck .~ initialDeck)
              . ( players
                    .~ [ ( (actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 0)
                             . (chips .~ 0)
                             . (committed .~ 2000)
                         )
                           player1,
                         ( (actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 2000)
                             . (committed .~ 4000)
                             . (chips .~ 0)
                         )
                           player3,
                         ( (playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 2000)
                             . (committed .~ 4000)
                             . (chips .~ 0)
                         )
                           player3
                       ]
                )
              $ initialGameState'
      everyoneAllIn flopGame `shouldBe` True

  describe "getHandRankings" $
    it "Number of hand rankings should equal number of active players" $ require prop_number_of_hand_rankings_and_active_players_same

  describe "doesPlayerHaveToAct" $ do
    it "should be False when posToAct is not on player" $ require prop_plyrShouldntActWhenNotInPos

    it "should be False when player has no chips" $ require prop_plyrShouldntActWhenNoChips

    it "should be False when not enough players (<2) during predeal to start a game" $
      require prop_plyrShouldntActDuringPreDealWhenNotEnoughPlyrsToStartGame

    it "should return True for an active player in position" $ do
      let game =
            (street .~ Flop) . (dealer .~ 0) . (currentPosToAct ?~ 1)
              . (players .~ [(chips .~ 1000) player5, (chips .~ 1000) player2])
              $ initialGameState'
      doesPlayerHaveToAct (_playerName player2) game `shouldBe` True
      doesPlayerHaveToAct (_playerName player5) game `shouldBe` False

      doesPlayerHaveToAct "player0" turnGameThreePlyrs `shouldBe` True

    it "should return False for non-active players" $ do
      let game =
            (street .~ Flop) . (dealer .~ 0)
              . ( players
                    .~ [ (chips .~ 1000) player5,
                         (playerState .~ Folded) player4,
                         (playerState .~ SatOut) player3,
                         (chips .~ 1000) player2
                       ]
                )
              $ initialGameState'
      doesPlayerHaveToAct (_playerName player3) game `shouldBe` False
      doesPlayerHaveToAct (_playerName player4) game `shouldBe` False

    describe "Heads Up Game" $
      describe "PreDeal" $ do
        describe "When 0 players sat in" $ do
          let game' =
                (street .~ PreDeal) . (maxBet .~ 0) . (pot .~ 0)
                  . (deck .~ initialDeck)
                  . (currentPosToAct ?~ 1)
                  . (dealer .~ 0)
                  . ( players
                        .~ [ ( (actedThisTurn .~ False) . (playerState .~ SatOut)
                                 . (bet .~ 0)
                                 . (chips .~ 2000)
                                 . (committed .~ 0)
                                 . (bet .~ 0)
                             )
                               player1,
                             ( (actedThisTurn .~ False) . (playerState .~ SatOut)
                                 . (bet .~ 0)
                                 . (committed .~ 0)
                                 . (bet .~ 0)
                                 . (chips .~ 2000)
                             )
                               player2
                           ]
                    )
                  $ initialGameState'

          it "No player should have to act first" $ do
            doesPlayerHaveToAct (_playerName player1) game' `shouldBe` False
            doesPlayerHaveToAct (_playerName player2) game' `shouldBe` False

        describe "When 1 player is sat in" $ do
          let game' =
                (street .~ PreDeal)
                  . ( players
                        .~ [ ( (actedThisTurn .~ False) . (playerState .~ SatOut)
                                 . (bet .~ 0)
                                 . (chips .~ 2000)
                                 . (committed .~ 0)
                             )
                               player1,
                             ( (actedThisTurn .~ False) . (playerState .~ In) . (bet .~ 0)
                                 . (committed .~ 0)
                                 . (chips .~ 2000)
                             )
                               player2
                           ]
                    )
                  $ initialGameState'

          it "No player should have to act first" $ do
            doesPlayerHaveToAct (_playerName player1) game' `shouldBe` False
            doesPlayerHaveToAct (_playerName player2) game' `shouldBe` False

        describe
          "When 2 players are both sat in but no one has posted a blind yet"
          $ do
            let game' =
                  (street .~ PreDeal) . (maxBet .~ 0) . (pot .~ 0)
                    . (deck .~ initialDeck)
                    . (currentPosToAct ?~ 1)
                    . (dealer .~ 0)
                    . ( players
                          .~ [ ( (actedThisTurn .~ False) . (playerState .~ In) . (bet .~ 0)
                                   . (chips .~ 2000)
                                   . (committed .~ 0)
                               )
                                 player1,
                               ( (actedThisTurn .~ False) . (playerState .~ In) . (bet .~ 0)
                                   . (committed .~ 0)
                                   . (chips .~ 2000)
                               )
                                 player2
                             ]
                      )
                    $ initialGameState'
            it "Player1 should not have to act" $
              doesPlayerHaveToAct (_playerName player1) game'
                `shouldBe` False

            it "Player2 should not have to act" $
              doesPlayerHaveToAct (_playerName player2) game'
                `shouldBe` False

        describe "When one player has already posted blinds" $ do
          let game' =
                (street .~ PreDeal) . (maxBet .~ 25) . (pot .~ 25)
                  . (deck .~ initialDeck)
                  . (currentPosToAct ?~ 1)
                  . (dealer .~ 0)
                  . ( players
                        .~ [ ( (actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 0)
                                 . (chips .~ 2000)
                                 . (committed .~ 25)
                             )
                               player1,
                             ( (actedThisTurn .~ False) . (playerState .~ In) . (bet .~ 0)
                                 . (committed .~ 0)
                                 . (chips .~ 1950)
                             )
                               player2
                           ]
                    )
                  $ initialGameState'

          it "Player1 should not have to act" $
            doesPlayerHaveToAct (_playerName player1) game'
              `shouldBe` False

          it "Player2 should have to act" $
            doesPlayerHaveToAct (_playerName player2) game'
              `shouldBe` True

        describe "PreFlop" $ do
          describe "First Turn" $ do
            let game' =
                  (street .~ PreFlop) . (maxBet .~ 50) . (deck .~ initialDeck)
                    . (smallBlind .~ 25)
                    . (smallBlind .~ 50)
                    . (pot .~ 100)
                    . (currentPosToAct ?~ 1)
                    . (dealer .~ 1)
                    . ( players
                          .~ [ ( (actedThisTurn .~ False) . (playerState .~ In)
                                   . (bet .~ 50)
                                   . (committed .~ 50)
                                   . (chips .~ 1950)
                               )
                                 player1,
                               ( (actedThisTurn .~ False) . (playerState .~ In)
                                   . (bet .~ 50)
                                   . (chips .~ 1975)
                                   . (committed .~ 50)
                               )
                                 player2
                             ]
                      )
                    $ initialGameState'
            it "Player1 should not have to act" $
              doesPlayerHaveToAct (_playerName player1) game'
                `shouldBe` False
            it "Player2 should have to act" $
              doesPlayerHaveToAct (_playerName player2) game'
                `shouldBe` True

          describe "Second Turn" $ do
            let game' =
                  (street .~ PreFlop) . (maxBet .~ 50) . (deck .~ initialDeck)
                    . (smallBlind .~ 25)
                    . (smallBlind .~ 50)
                    . (pot .~ 100)
                    . (currentPosToAct ?~ 1)
                    . (dealer .~ 1)
                    . ( players
                          .~ [ ( (actedThisTurn .~ True) . (playerState .~ In)
                                   . (bet .~ 50)
                                   . (committed .~ 50)
                                   . (chips .~ 1950)
                               )
                                 player1,
                               ( (actedThisTurn .~ False) . (playerState .~ In)
                                   . (bet .~ 50)
                                   . (chips .~ 1975)
                                   . (committed .~ 50)
                               )
                                 player2
                             ]
                      )
                    $ initialGameState'

            it "Player1 should not have to act" $
              doesPlayerHaveToAct (_playerName player1) game'
                `shouldBe` False

            it "Player2 should have to act" $
              doesPlayerHaveToAct (_playerName player2) game'
                `shouldBe` True

          describe "Third Turn" $ do
            let game' =
                  (street .~ PreFlop) . (maxBet .~ 50) . (pot .~ 0)
                    . (deck .~ initialDeck)
                    . (currentPosToAct ?~ 0)
                    . (dealer .~ 1)
                    . ( players
                          .~ [ ( (playerState .~ In) . (bet .~ 0) . (committed .~ 50)
                                   . (chips .~ 1950)
                               )
                                 player1,
                               ( (actedThisTurn .~ True) . (playerState .~ In)
                                   . (bet .~ 25)
                                   . (chips .~ 1950)
                                   . (committed .~ 50)
                                   . (actedThisTurn .~ False)
                               )
                                 player2
                             ]
                      )
                    $ initialGameState'
            it "Player1 should have to act" $
              doesPlayerHaveToAct (_playerName player1) game'
                `shouldBe` True
            it "Player2 should not have to act" $
              doesPlayerHaveToAct (_playerName player2) game'
                `shouldBe` False

        describe "Flop" $ do
          describe "First turn" $ do
            let game' =
                  (street .~ Flop) . (maxBet .~ 0) . (pot .~ 100)
                    . (deck .~ initialDeck)
                    . (currentPosToAct ?~ 1)
                    . (dealer .~ 0)
                    . ( players
                          .~ [ ( (actedThisTurn .~ False) . (playerState .~ In)
                                   . (bet .~ 0)
                                   . (chips .~ 2000)
                                   . (committed .~ 50)
                               )
                                 player1,
                               ( (actedThisTurn .~ False) . (playerState .~ In)
                                   . (bet .~ 0)
                                   . (committed .~ 50)
                                   . (chips .~ 2000)
                               )
                                 player2
                             ]
                      )
                    $ initialGameState'
            it "Player1 should not have to act" $
              doesPlayerHaveToAct (_playerName player1) game'
                `shouldBe` False
            it "Player2 should have to act" $
              doesPlayerHaveToAct (_playerName player2) game'
                `shouldBe` True

          describe "Second turn" $ do
            let game' =
                  (street .~ Flop) . (maxBet .~ 0) . (pot .~ 100)
                    . (deck .~ initialDeck)
                    . (currentPosToAct ?~ 0)
                    . (dealer .~ 0)
                    . ( players
                          .~ [ ( (actedThisTurn .~ False) . (playerState .~ In)
                                   . (bet .~ 0)
                                   . (chips .~ 2000)
                                   . (committed .~ 50)
                               )
                                 player1,
                               ( (actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 0)
                                   . (committed .~ 50)
                                   . (chips .~ 2000)
                               )
                                 player2
                             ]
                      )
                    $ initialGameState'
            it "Player1 should have to act" $
              doesPlayerHaveToAct (_playerName player1) game'
                `shouldBe` True
            it "Player2 should not have to act" $
              doesPlayerHaveToAct (_playerName player2) game'
                `shouldBe` False

  describe "nextPosToAct" $ do
    let player1 =
          Player
            { _pockets = Nothing,
              _chips = 2000,
              _bet = 0,
              _playerState = In,
              _playerName = "player1",
              _committed = 100,
              _actedThisTurn = True,
              _possibleActions = []
            }

        player2 =
          Player
            { _pockets = Nothing,
              _chips = 2000,
              _bet = 0,
              _playerState = Folded,
              _playerName = "player2",
              _committed = 50,
              _actedThisTurn = False,
              _possibleActions = []
            }

        player3 =
          Player
            { _pockets = Nothing,
              _chips = 2000,
              _bet = 0,
              _playerState = In,
              _playerName = "player3",
              _committed = 50,
              _actedThisTurn = False,
              _possibleActions = []
            }

        player4 =
          Player
            { _pockets = Nothing,
              _chips = 2000,
              _bet = 0,
              _playerState = In,
              _playerName = "player3",
              _committed = 0,
              _actedThisTurn = False,
              _possibleActions = []
            }

        player5 =
          Player
            { _pockets = Nothing,
              _chips = 4000,
              _bet = 4000,
              _playerState = In,
              _playerName = "player5",
              _committed = 4000,
              _actedThisTurn = True,
              _possibleActions = []
            }

    it "nextPosToAct is always less than player count" $ require prop_nextPosToActLTPlayerCount

    it "When everyone is all in then there should be no next player to act" $ require prop_when_everyone_allIn_next_pos_Nothing

    it "When awaiting player action nextPosToAct should never be Nothing" $ require prop_when_awaiting_action_nextPos_always_Just

    describe "Heads Up" $
      it "should modulo increment position for two players who are both In" $ do
        let game =
              (street .~ PreFlop) . (currentPosToAct ?~ 0)
                . (players .~ [player1, player3])
                $ initialGameState'
        nextPosToAct game `shouldBe` Just 1
        let game2 =
              (street .~ PreFlop) . (currentPosToAct ?~ 1)
                . (players .~ [player1, player3])
                $ initialGameState'
        nextPosToAct game2 `shouldBe` Just 0

    describe "Three Players" $ do
      let threePGamePreFlop =
            Game
              { _dealer = 2,
                _currentPosToAct = Just 0,
                _smallBlind = 25,
                _bigBlind = 50,
                _minBuyInChips = 1500,
                _maxBuyInChips = 3000,
                _pot = 100,
                _maxBet = 50,
                _street = PreFlop,
                _winners = NoWinners,
                _board = [],
                _maxPlayers = 6,
                _waitlist = [],
                _deck = Deck [],
                _players =
                  [ Player
                      { _pockets = Nothing,
                        _chips = 2300,
                        _bet = 50,
                        _playerState = In,
                        _playerName = "player0",
                        _committed = 50,
                        _actedThisTurn = True,
                        _possibleActions = []
                      },
                    Player
                      { _pockets = Nothing,
                        _chips = 1700,
                        _bet = 50,
                        _playerState = In,
                        _playerName = "player1",
                        _committed = 50,
                        _actedThisTurn = True,
                        _possibleActions = []
                      },
                    Player
                      { _pockets = Nothing,
                        _chips = 2122,
                        _bet = 0,
                        _playerState = Folded,
                        _playerName = "player2",
                        _committed = 0,
                        _actedThisTurn = True,
                        _possibleActions = []
                      }
                  ]
              }

      it "Next position at end of PreDeal (PreFlop) should should skip folded player's position" $
        nextPosToAct threePGamePreFlop `shouldBe` Just 1

      it "Next position at end of PreDeal (PreFlop) should be left of big blind's (dealer's) position" $ do
        let game2 =
              (street .~ PreFlop) . (currentPosToAct ?~ 2)
                . (players .~ [player1, player1, player1])
                $ initialGameState'
        nextPosToAct game2 `shouldBe` Just 1

      it "Next position at end of Flop (Turn) should be small blind's position" $ do
        let game2 =
              (street .~ Flop) . (currentPosToAct ?~ 0)
                . (players .~ [player1, player1, player1])
                $ initialGameState'
        nextPosToAct game2 `shouldBe` Just 1

      it "should modulo increment position when one player has folded" $ do
        let game2 =
              (street .~ PreFlop) . (currentPosToAct ?~ 2)
                . (players .~ [player1, player2, player3])
                $ initialGameState'
        nextPosToAct game2 `shouldBe` Just 0

    describe "Four Players" $
      it "should modulo increment position for four players" $ do
        let game =
              (street .~ PreFlop) . (currentPosToAct ?~ 2)
                . (players .~ [player1, player4, player3, player2])
                $ initialGameState'
        nextPosToAct game `shouldBe` Just 0
        let game2 =
              (street .~ PreFlop) . (currentPosToAct ?~ 2)
                . ( players
                      .~ [ player1,
                           player4,
                           player3,
                           (playerState .~ In) player2,
                           (playerState .~ In) player2
                         ]
                  )
                $ initialGameState'
        nextPosToAct game2 `shouldBe` Just 3
        let game3 =
              (street .~ PreFlop) . (currentPosToAct ?~ 2)
                . (players .~ [player2, player4, player3, player2])
                $ initialGameState'
        nextPosToAct game3 `shouldBe` Just 1
