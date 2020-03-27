import React from 'react'

import ActionPanel from './ActionPanel'
import Board from './Board.tsx'
import Seat from './Seat'
import Card from './Card'

import { fromJS, toJS, List } from 'immutable'

let isEveryoneAllIn = players => {
  let activesCount = players.filter(p =>
    p.get("_playerState") == "In").size
  let allInCount = players.filter(p =>
    (p.get("_playerState") == "In") && (p.get("_chips") === 0)).size

  console.log('actives count', activesCount)
  console.log('all in count', allInCount)
  if (activesCount < 2) {
    return false;
  }
  else {
    let notAllInCount = activesCount - allInCount
    return notAllInCount <= 1
  }
}



const getSeatedPlayer = (
  username,
  player,
  gameStage,
  position,
  isTurnToAct,
  isEveryoneAllIn,
  activePlayerCount,

) => (
    <Seat
      key={position}
      position={position}
      isEveryoneAllIn={isEveryoneAllIn}
      playerName={player.get('_playerName')}
      chips={player.get('_chips')}
      hasPocketCards={
        player.get('_playerName') !== username &&
        player.get('_playerState') === 'In' &&
        gameStage !== 'PreDeal' &&
        gameStage !== 'Showdown'
      }
      playerState={player.get('_playerState')}
      activePlayerCount={activePlayerCount}
      isTurnToAct={isTurnToAct && (player.get('_actedThisTurn') === false) && gameStage !== 'Showdown'}
    />
  )

const getSeats = (username, maxPlayers, players, gameStage, currentPosToAct, isEveryoneAllIn, activePlayerCount) =>
  Array(maxPlayers)
    .fill(null)
    .map((_, i) => {
      const player = players.get(i)
      const isTurnToAct = i === currentPosToAct

      return player ? (
        getSeatedPlayer(username, player, gameStage, i, isTurnToAct, isEveryoneAllIn, activePlayerCount)
      ) : (
          <Seat
            key={i}
            position={i} />
        )
    })

const getPocketCards = players =>
  players.map((p, i) => {
    if (p.get('_pockets') === null) {
      return ''
    }

    return p.get('_pockets').length !== 0 ? (
      <div className={`showdown-pocket-cards-${i}`} key={p.get('_playerName')}>
        <div className="showdown-pocket-cards-container">
          {p.get('_pockets').map(card => {
            const rank = card.get('rank')
            const suit = card.get('suit')

            return <Card key={rank + suit} rank={rank} suit={suit} />
          })}
        </div>
      </div>
    ) : (
        ''
      )
  })

const getPlayerBets = players =>
  players.map(
    (p, i) =>
      p.get('_bet') > 0 ? (
        <div className={`player-bet-container-pos-${i}`}>
          <div className={`player-bet-pos-${i}`}>
            <div className="player-bet-chip" />
            <div className="player-bet-label">
              <h3><span className="monospaced-font-bold">{`$${p.get('_bet')}`}</span></h3>
            </div>
          </div>
        </div>
      ) : (
          ''
        )
  )

const parseAvailableActions = actions => {
  console.log(actions)


  if (List.size === 0) {
    return actions
  } else {
    const actionsJS = actions.toJS()
    return actionsJS.map(a => {
      console.log(a)


      if (a.tag === "PostBlind") {
        if (a.contents === "Big") {
          return "PostBigBlind"
        } else {
          return "PostSmallBlind"
        }
      } else {
        return a.tag
      }
    })
  }
}

const Game = props => {
  const { game, username, isTurnToAct } = props




  if (game) {
    const jsgame = game.toJS()
    console.log(jsgame)
    console.log('everyone all in ', isEveryoneAllIn(game.get("_players")))
    const everyoneAllIn = isEveryoneAllIn(game.get("_players"))
    const players = game.get('_players')
    const dealerPos = game.get('_dealer')
    const maxPlayers = 6
    const gameStage = game.get('_street')
    const potSize = game.get('_pot')

    let activePlayerCount = players.filter(p =>
      p.get("_playerState") == "In").size

    const userPlayer = game
      .get('_players')
      .find(p => p.get('_playerName') === username)


    const userPocketCards = userPlayer ? userPlayer.get('_pockets') : null
    const userAvailableActions = userPlayer ? userPlayer.get('_possibleActions') : fromJS([])
    console.log(((parseAvailableActions(userAvailableActions))).toJS)

    const currentPosToAct = game.get('_currentPosToAct')
    const isMultiplayerShowdown =
      game.get('_winners').get('tag') == 'MultiPlayerShowdown'
    const showdownPots = game.get('_winners').get('contents')
    const mainShowdownPot = showdownPots
      ? showdownPots.get
        ? showdownPots.get(0)
        : null
      : null
    const mainShowdownPotHandRanking = mainShowdownPot
      ? mainShowdownPot.get(0).get(0)
      : null
    const mainShowdownPotHandCards = mainShowdownPot
      ? mainShowdownPot.get(0).get(1)
      : null
    const mainShowdownPotHandPlayers = mainShowdownPot
      ? mainShowdownPot.get(1)
      : null
    const playerNamesWinnersOfMainShowdownPot = mainShowdownPot
      ? mainShowdownPot.get(1)
      : null


    return (
      <div className="game-view-grid">
        <div className="game-container">
          <div className="table-container">
            {getPocketCards(players)}
            {getSeats(
              username,
              maxPlayers,
              players,
              gameStage,
              currentPosToAct,
              everyoneAllIn,
              activePlayerCount,
              gameStage
            )}
            <div className="game-grid">
              {players ? (
                players.count() > 1 ? (
                  <div className={`dealer-btn-pos-${dealerPos}`}>D</div>
                ) : (
                    ''
                  )
              ) : (
                  ''
                )}
              <Board cards={game.get('_board')} />
              {getPlayerBets(players)}
              <h3 className="pot-label">
                <span className="monospaced-font-bold">{`$${potSize}`}</span>
              </h3>
              {mainShowdownPot ? (
                <h3 className="winners-label">
                  {`${mainShowdownPotHandPlayers} wins with ${mainShowdownPotHandRanking}`}
                </h3>
              ) : (
                  ''
                )}
            </div>
            <div className="game-table" />
          </div>
        </div>
        <ActionPanel
          {...props}

          gameStage={gameStage}
          userPocketCards={userPocketCards}
          availableActions={parseAvailableActions(userAvailableActions)}
          userPlayer={userPlayer}
          maxCurrBet={game.get("_maxBet")}
          bigBlind={game.get("_bigBlind")}
        />
      </div>
    )
  }
  return <h2>Loading Game...</h2>
}

export default Game
