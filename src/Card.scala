

/*
 * @author jk
 */
object Card {
  val possibleCards : Map[String,Int] = Map("2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9, "10" -> 10, "J" -> 11, "Q" -> 12, "K" -> 13, "A" -> 14)
  val suits : List[String] = List("C", "D", "H", "S")
  def getValueOf(name : String) : Int = {
    // throws exception if not found...
    possibleCards(name)
  }
  def getNameFromValue (value : Int) : String = {
    var name : String = ""
    if (value > 10 || value < 2) {
      name = value match {
        case 11 => "J"
        case 12 => "Q"
        case 13 => "K"
        case 14 => "A"
        case _ => throw new java.util.NoSuchElementException()
      }
    } else {
      name = value.toString
    }
    return name
  }
}

class Card(val value : Int, val name : String, val suit : String) extends Ordered[Card] {
  def this(name : String, suit : String) = {
    this(Card.getValueOf(name), name, suit)
  }
  def this(value : Int, suit : String) = {
    this(value, Card.getNameFromValue(value), suit)
  }
  override def toString = name + suit
  override def compare (that: Card) = {
    if (this.suit == that.suit)
      this.value.compare(that.value)
    else
      if (that.suit == "S") -1 else 1
  }
  override def equals(that:Any) : Boolean = {
    that.isInstanceOf[Card] && 
    this.value == that.asInstanceOf[Card].value && 
    this.suit == that.asInstanceOf[Card].suit
  }
}