

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.util.Random

/**
 * @author jk
 */
class Deck extends Stack[Card] {
  buildDeck
  def buildDeck = {
    var tempcards = new ListBuffer[Card]
    for (card <- Card.possibleCards.keySet; suit <- Card.suits) {
      tempcards += new Card(card, suit)
    }
    for(card <- Random.shuffle(tempcards)) {
      push(card)
    }
  }
}