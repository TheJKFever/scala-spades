

import scala.collection.mutable.TreeSet


/**
 * @author jk
 */

class Hand extends TreeSet[Card]()(Ordering[(String, Int)].on[Card] { x => (x.suit, x.value) }) {
  def spades : List[Card] = bySuit("S")
  def diamonds : List[Card] = bySuit("D")
  def hearts : List[Card] = bySuit("H")
  def clubs : List[Card] = bySuit("C")
  def bySuit(suit : String) : List[Card] = {
    (filter(_.suit==suit)).toList;
  }
  def objectiveScore : Int = {
    // Add a point for every King and Ace
    var score : Int = filter(_.value >= 13).size
    // Add a point for other spades if have less than 3 cards of other suit
    var otherSpadesAccountedFor : Int = spades.filter(_.value < 13).size
    if (otherSpadesAccountedFor > 0 && diamonds.size < 3) {
      score += 1
      otherSpadesAccountedFor -= 1
    }
    if (otherSpadesAccountedFor > 0 && hearts.size < 3) {
      score += 1
      otherSpadesAccountedFor -= 1
    }
    if (otherSpadesAccountedFor > 0 && clubs.size < 3) {
      score += 1
      otherSpadesAccountedFor -= 1
    }
    return score
  }
  
//  def remove(card : Card) : Option[Card] = {
//    var index = indexOf(card)
//    if (index == -1) return None
//    else {
//      val card = this(index)
//      val (start, end) = splitAt(index)
//      clear
//      this ++= (start ++= end.tail)
//      return Some(card)
//    }
//  }
  
  def findByValueSuit(input : String) : Option[Card] = {
    this.find { x => x.toString() == input }
//    val seq = this.toIndexedSeq
//    var index = seq.map(_.toString).indexOf(input)
//    if ( index != -1 ) Some(seq(index)) else None
  }
  
  override def toString : String = {
    this.mkString(", ")
//    this.sortBy { x => (x.suit, x.value) } mkString(", ") // from MutableList
  }
}