package bowling
import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random

object Bowling extends App{

  val random = new Random()
  mainLoop(GameState(null), random)

  def mainLoop(gameState: GameState, random: Random): Unit = {
    println("Bienvenue sur le jeu de bowling")
    val username = readLine("Veuillez rentrer votre pseudo")
    mainGameLoop(gameState.copy(player = Player(username= username, random = random)))
  }

  @tailrec
  def mainGameLoop(gameState: GameState): Unit = {
    println("\nFrame : " + gameState.player.current_frame)
    val endGameState = gameLoop(gameState.copy(player = Player(username= gameState.player.username, random = gameState.player.random)))
    println("Partie terminée, votre score est de : " + endGameState.player.score)
    val playAgain = readLine("Voulez vous rejouer ? Y or N")
    playAgain match {
      case "Y" =>
        mainGameLoop(gameState = gameState.copy(player = gameState.player.reset()))
      case _ =>
    }
  }

  @tailrec
  def gameLoop(gameState: GameState): GameState = {
    if(gameState.player.current_frame<=10) {
      if(gameState.player.canRollOnTheCurrentFrame) {
        val roll = gameState.player.roll()
        if(roll.isDefined) {
          val newPlayer = gameState.player.updateScore(roll.get)
          if(newPlayer.isDefined) {
            println("You rolled " + roll.get)
            if(newPlayer.get.current_frame < 10) {
              if (newPlayer.get.frames.find(frame => frame.nb_frame == newPlayer.get.current_frame).get.state
                == State.STRIKE) {
                println("STRIKE")
              } else if (newPlayer.get.frames.find(frame => frame.nb_frame == newPlayer.get.current_frame).get.state
                == State.SPARE) {
                println("SPARE")
              }
            } else {
              val currentFrame = newPlayer.get.frames.find(frame => frame.nb_frame == newPlayer.get.current_frame).get
              if (currentFrame.state == State.STRIKE) {
                currentFrame.rolls.size match {
                  case 1 => println("STRIKE")
                  case 2 => if(currentFrame.rolls(1) == 10) println("STRIKE")
                  case 3 => if(currentFrame.rolls(2) == 10) println("STRIKE")
                            else if(currentFrame.rolls(1) + currentFrame.rolls(2) == 10 && currentFrame.rolls(1) != 10)
                            println("SPARE")
                  case _ =>
                }
              } else if (currentFrame.state == State.SPARE) {
                currentFrame.rolls.size match {
                  case 2 => if(currentFrame.rolls(0) + currentFrame.rolls(1) == 10) println("SPARE")
                  case _ =>
                }
              }
            }
            gameLoop(gameState.copy(player = newPlayer.get))
          } else {
            gameLoop(gameState)
          }
        } else {
          gameLoop(gameState)
        }
      } else {
        val newPlayer = gameState.player.nextFrame()
        if(newPlayer.isDefined) {
          println("Score: " + newPlayer.get.score)
          println("\nFrame : " + newPlayer.get.current_frame)
          gameLoop(gameState.copy(player = newPlayer.get))
        } else {
          gameState
        }
      }
    } else {
      gameState
    }
  }
}
