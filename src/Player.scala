
/**
 * @author jk
 */
class Player (val name : String) {
  var hand : Hand = new Hand
  var bet : Option[Int] = None
  var tricks : Int = 0
  var isReady : Boolean = false
  var teammate : Player = null
  var toLeft : Player = null
  var toRight : Player = null
  def deal(card : Card) = { hand += card }
  def next : Player = { toLeft }
  def getBet : String = {
    bet match {
      case Some(x) => x.toString
      case None => "_"
    }
  }
    
  override def toString : String = {
    name
  }
  
  def stats : String = {
    name + " - Bet: " + getBet + ", tricks: " + tricks
  }
}