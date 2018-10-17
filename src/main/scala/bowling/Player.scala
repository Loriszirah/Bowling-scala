package bowling

import scala.util.Random

case class Player(username: String = "unknown", score: Int = 0, current_frame: Int = 1, random: Random,
                  bonusRoll: List[Int] = List(0,0), frames: List[Frame] = List(Frame(nb_frame = 1))) {
  /**
    * Choose a random int corresponding to the number of pins touched
    * @return None if the player can't roll anymore on the current frame, Some otherwise where the int represent
    *         the number of pins touched
    */
  def roll(): Option[Int] = {
    val frame = this.frames.find(frame => frame.nb_frame == this.current_frame).get
    if(frame.nb_frame <10) {
      if (frame.rolls.isEmpty) {
        Some(this.random.nextInt(11))
      } else if (frame.rolls.size == 1) {
        Some(this.random.nextInt(11 - frame.rolls.head))
      } else None
    } else { // 10th Frame
      if (frame.rolls.isEmpty) {
        Some(this.random.nextInt(11))
      } else if (frame.rolls.size == 1) {
        if(frame.rolls.head == 10) {
          Some(this.random.nextInt(11))
        } else {
          Some(this.random.nextInt(11 - frame.rolls.head))
        }
      } else if (frame.state == State.SPARE || frame.state == State.STRIKE) {
        Some(this.random.nextInt(11))
      } else None
    }
  }

  /**
    * Answer if the player can roll again in the current frame. The case of the triple roll for the last frame needs to
    * be taking in consideration
    * @return True if he can, False otherwise
    */
  def canRollOnTheCurrentFrame: Boolean = {
    if(this.current_frame < 10) {
      val frame = this.frames.find(frame => frame.nb_frame == this.current_frame)
      if(frame.isDefined){
        if(frame.get.state != State.NOTHING) {
          false
        } else {
          frame.get.rolls.size < 2
        }
      } else false
    } else {
      val frame = this.frames.find(frame => frame.nb_frame == this.current_frame)
      if(frame.isDefined) {
        if(frame.get.state == State.NOTHING){
          if(frame.get.rolls.size == 2) {
            false
          } else {
            true
          }
        } else {
          frame.get.rolls.size < 3
        }
      } else false
    }
  }

  /**
    * Update the score of the player regarding to the nb_pins but also by taking in consideration the bonus.
    * @param nb_pins the number of pints touched
    * @return None if the player can't be updated, Some with the player updated (new score, frames, bonus)
    */
  def updateScore(nb_pins: Int): Option[Player] = {
    val newCurrentFrame = this.frames.find(frame => frame.nb_frame == this.current_frame).get.roll(nb_pins)
    if(newCurrentFrame.isDefined){
      val newFrames = this.frames.map(frame => if(frame.nb_frame==this.current_frame) {newCurrentFrame.get} else frame)
      var newScore = 0
      if (this.current_frame < 10 || newCurrentFrame.get.rolls.size == 1) {
        newScore = this.score + nb_pins + this.bonusRoll.head * nb_pins
      } else if(newCurrentFrame.get.state != State.STRIKE && newCurrentFrame.get.rolls.size == 2){
        newScore = this.score + nb_pins + this.bonusRoll.head * nb_pins
      } else {
        newScore = this.score + this.bonusRoll.head * nb_pins
      }
      if (nb_pins == 10 && newCurrentFrame.get.rolls.size == 1) {
        val newBonusRoll = List(this.bonusRoll.tail.head + 1, 1)
        Some(this.copy(frames = newFrames, bonusRoll = newBonusRoll, score = newScore))
      } else {
        if (newCurrentFrame.get.rolls.size == 2) {
          if (newCurrentFrame.get.rolls.head + newCurrentFrame.get.rolls.tail.head == 10) {
            val newBonusRoll = List(this.bonusRoll.tail.head + 1, 0)
            Some(this.copy(frames = newFrames, bonusRoll = newBonusRoll, score = newScore))
          } else if(newCurrentFrame.get.rolls.head + newCurrentFrame.get.rolls.tail.head > 10 && newCurrentFrame.
            get.state != State.STRIKE) {
            None
          }else {
            Some(this.copy(frames = newFrames, bonusRoll = List(this.bonusRoll.tail.head, 0), score = newScore))
          }
        } else {
          Some(this.copy(frames = newFrames, bonusRoll = List(this.bonusRoll.tail.head, 0), score = newScore))
        }
      }
    } else None
  }

  /**
    * Add to the player the next Frame
    * @return None if there isn't more frame, Some with the updated player otherwise
    */
  def nextFrame(): Option[Player] = {
    if(this.current_frame<10)
      Some(this.copy(frames = this.frames :+ Frame(nb_frame = this.current_frame+1), current_frame = this.current_frame+1))
    else None
  }

  /**
    * Reset the attributes of the player but keep the username and the random
    * @return the new player reset
    */
  def reset(): Player = Player(username = this.username, random = this.random)
}
