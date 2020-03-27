import React, {Component, ReactComponentElement} from 'react';

import Card from './Card';
import CardModel from '../data/card.model';

export default class Board extends Component<any> {

  public render(): ReactComponentElement<any> {
    const cards: CardModel[] = this.props.cards;

    console.log("cards", cards);

    return (
      <div className='board-cards'>
        <div className='board-cards-container'>
          {
            cards.map(card => {
              return (
                <Card
                  key={card.rank + card.suit}
                  rank={card.rank}
                  suit={card.suit}
                />
              )
            })
          }
        </div>
      </div>
    )
  }
}
