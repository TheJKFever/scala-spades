
import scala.collection.mutable.LinkedHashMap

/**
 * @author jk
 */
class Trick extends LinkedHashMap[Player, Card] {
  def getWinner : Player = {
    var tempMax : Player = this.head._1
    for (player <- this.keys) {
      if (this(tempMax).compare(this(player)) < 0 ) tempMax = player
    }
    tempMax
  }
  
  def printTableFor(player: Player) : String = {
    "              " + this.getOrElse(player.teammate, "__") + "                 \n" +
    "       " + this.getOrElse(player.toLeft, "__") + "            " + this.getOrElse(player.toRight, "__") + "\n" +
    "              " + this.getOrElse(player, "__") + "                 \n"
  }
}