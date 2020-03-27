import React, {Component, ReactComponentElement} from 'react';

import clubs from '../../static/Clubs.svg'
import hearts from '../../static/Hearts.svg'
import spades from '../../static/Spades.svg'
import diamonds from '../../static/Diamonds.svg'

const showRank = rank => {
  switch (rank) {
    case 'Ace':
      return 'A'
    case 'King':
      return 'K'
    case 'Queen':
      return 'Q'
    case 'Jack':
      return 'J'
    case 'Ten':
      return '10'
    case 'Nine':
      return '9'
    case 'Eight':
      return '8'
    case 'Seven':
      return '7'
    case 'Six':
      return '6'
    case 'Five':
      return '5'
    case 'Four':
      return '4'
    case 'Three':
      return '3'
    case 'Two':
      return '2'
  }
}

const suitSVG = suit => {
  switch (suit) {
    case 'Spades':
      return spades
    case 'Diamonds':
      return diamonds
    case 'Hearts':
      return hearts
    case 'Clubs':
      return clubs
  }
}

export default class Card extends Component {
  render() {
    return (
      <div className='card'>
        <div className="rank">
          <span className="monospace-font-bold">{showRank(this.props.rank)}</span>
        </div>
        <div className="suit">
          <img alt={this.props.suit} src={suitSVG(this.props.suit)}/>
        </div>
      </div>
    );
  }
}

