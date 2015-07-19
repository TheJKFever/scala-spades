
import scala.collection.mutable.ArrayBuffer

/**
 * @author jk
 */
class Team {
  val players = new ArrayBuffer[Player]
  var score = 0
  var bags = 0
  var bet : Option[Int] = None
  
  def getBet : String = {
    bet match {
      case None => "_"
      case Some(x) => x.toString
    }
  }
  
  def updateScore = {
    val tricks = players(0).tricks + players(1).tricks
//    bet > tricks fail!
//    find out bags
    bet match {
      case Some(bet) => {
        players.foreach { x => if (x.bet == 0) {
            if (x.tricks == 0) score += 100
            else score -= 100
          }
        }
        if (bet > tricks) score -= (bet*10)
        else {
          score += (bet*10)
          bags += (tricks-bet)
          while (bags > 10) { // Should never while...
            score -= 90
            bags -= 10
          }
        }
      }
      case None => throw new Exception("Don't ever call `updateScore` when you haven't set the bet")
    }
  }
  
  def setTeamBets = {
    players.foreach { x => x.bet match {
      case None => throw new Exception("Please set the players bets before calling setTeamBets")
      case Some(x) => x
      }
    }
    bet = Some((players(0).bet.get + players(1).bet.get))
  }
  
  override def toString : String = {
    "Players: " + players.map(x => x.name).mkString(", ") + "\n" +
    "        Score: " + (Math.signum(score)*(Math.abs(score)+bags)) +" Bet: " + getBet
  }
  
  def stats : String = {
    return this.toString
  }
}