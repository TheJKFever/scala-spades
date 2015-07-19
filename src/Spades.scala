

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * @author jk
 */
class Spades {
  var players = new ArrayBuffer[Player]
  var deck = new Deck
  var team1 = new Team
  var team2 = new Team
  var isSpadesBroken : Boolean  = false
  var leader : Player = null

  def isGameOver : Boolean = {
    (team1.score > 500 || team2.score > 500)
  }
  
  def isEveryoneReady : Boolean = {
    if (players.size < 4) return false
    for (player <- players) {
      if (!player.isReady) return false
    }
    return true
  }

  def clearPlayersHands = {
    for (player <- players) { player.hand.clear }
  }

  def dealCards(dealer : Player) = {
    var player = dealer.toLeft
    while (deck.nonEmpty) {
      player.deal(deck.pop)
      player = player.next
    }
  }

  def showTable(player : Player, trick : Option[Trick]) = {
    println()
    println("Team1 - " + team1.stats)
    println("Team2 - " +team2.stats)
    println("Left    : " + player.toLeft.stats)
    println("Teammate: " + player.teammate.stats)
    println("Right   : " + player.toRight.stats)
    println("You     : " + player.stats)
    trick match {
      case None => 
      case Some(trick) => println(trick.printTableFor(player))
    }
    print("Your hand: ")
    println(player.hand)
  }
  
  def possibleMoves(hand : Hand, trick : Trick) : List[Card] = {
    if (hand.size == hand.spades.size) return hand.spades
    else if (trick.isEmpty) {
      if (this.isSpadesBroken) return hand.toList // 1st card and spades broken
      else return hand.filter(_.suit != "S").toList // 1st card, can't lay spades
    } else {
      var sameSuit = hand.filter(_.suit == trick.head._2.suit).toList
      if (sameSuit.isEmpty) return hand.toList // don't have any of this suit
      else return sameSuit // can only lay same suit
    }
  }
  
  def takeBets(dealer : Player) = {
    var player = dealer.toLeft
    for (i <- 0 until 4) {
      takeBet(player)
      player = player.toLeft
    }
    team1.setTeamBets
    team2.setTeamBets
  }

  def takeBet(player : Player) = {
    showTable(player, None)
    println("Objective recommendation: " + player.hand.objectiveScore)
    print("Please enter your bet " + player.name + ": ")
    val bet = Console.readInt
    player.bet = Some(bet)
  }
  
  def resetRound = {
    team1.updateScore
    team2.updateScore
    isSpadesBroken = false
    team1.bet = None
    team2.bet = None
    for (player <- players) {
      player.bet = None
      player.tricks = 0
    }
  }
  
  def playRound(leader : Player) : Player = {
    // Consider making new class for Trick
    var trick = new Trick
    var currentPlayer = leader
    do {
      playTurn(currentPlayer, trick)
      currentPlayer = currentPlayer.toLeft
    } while (currentPlayer != leader)
    trick.getWinner
  }
  
  def playTurn(player : Player, trick : Trick) = {
    showTable(player, Some(trick))
    var selectedCard : Option[Card] = None
    do {
      println("Possible moves: " + possibleMoves(player.hand, trick).mkString(", "))
      println(s"${player.name}, Please choose a card to play: ")
      val input = readLine.trim()
      selectedCard = player.hand.findByValueSuit(input)
    } while(selectedCard == None || !possibleMoves(player.hand, trick).contains(selectedCard.get))
    if (selectedCard.get.suit == "S") isSpadesBroken = true
      
    player.hand.remove(selectedCard.get)
    trick(player) = selectedCard.get
  }

  def initializeTeams = {
    if (players.size != 4) throw new Exception("Please create exactly 4 players before calling `initializeTeams")
    for (i <- 0 until 4) {
      if (i%2==0) team1.players += players(i) else team2.players += players(i)
      players(i).toLeft = players((i+1)%4)
      players(i).teammate = players((i+2)%4)
      players(i).toRight = players((i+3)%4)
    }
  }
}

object Spades {
  def main(args : Array[String]) {
    var game = new Spades
    while (!game.isEveryoneReady) {
      print("Please enter player name : ")
      val name = Console.readLine
      var player = new Player(name)
      player.isReady = true
      game.players += player
      println(s"Welcome to the game $name")
    }
    game.initializeTeams

    var dealer : Player = Random.shuffle(game.players.toList).head
    while(!game.isGameOver) {
      game.clearPlayersHands
      dealer = dealer.toLeft
      game.deck = new Deck
      game.dealCards(dealer)
      game.takeBets(dealer)
      var player = dealer.toLeft
      while(dealer.hand.nonEmpty) {
        player = game.playRound(player)
        player.tricks += 1
      }
      game.resetRound
    }
  }  
}