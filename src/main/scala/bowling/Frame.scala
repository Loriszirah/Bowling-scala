package bowling

object State extends Enumeration {
  val STRIKE, SPARE, NOTHING = Value
}

case class Frame(rolls: List[Int] = List(), state: State.Value = State.NOTHING, nb_frame: Int) {
  /**
    * Update the frame regarding to the number of pins touched. If pins is equal to 10 at the first try, it should change
    * the state to STRIKE. If the sum of the 2 pins is equal to 10, it should change the state to SPARE. Otherwise, the
    * state will be NOTHING. For the 10th frame, 3 shots are possible unless the 2 rolls are not a STRIKE or a SPARE
    * @param nb_pins the number of pins touched in this frame
    * @return None if the frame can't be updated, Some of the updated frame with the given nb_pins otherwise
    */
  def roll(nb_pins: Int): Option[Frame] = {
    if(this.nb_frame < 10) {
      if (this.rolls.isEmpty) {
        if (nb_pins == 10) {
          Some(this.copy(rolls = rolls :+ nb_pins, state = State.STRIKE))
        } else {
          Some(this.copy(rolls = rolls :+ nb_pins))
        }
      } else if (rolls.size == 1) {
        if (rolls.head + nb_pins == 10) {
          Some(this.copy(rolls = rolls :+ nb_pins, state = State.SPARE))
        } else if(rolls.head + nb_pins > 10) {
          None
        } else{
          Some(this.copy(rolls = rolls :+ nb_pins))
        }
      } else None
    } else {
      if (this.rolls.isEmpty) {
        if (nb_pins == 10) {
          Some(this.copy(rolls = rolls :+ nb_pins, state = State.STRIKE))
        } else {
          Some(this.copy(rolls = rolls :+ nb_pins))
        }
      } else if (rolls.size == 1) {
        if (this.state == State.STRIKE) {
          Some(this.copy(rolls = rolls :+ nb_pins))
        } else {
          if (rolls.head + nb_pins == 10) {
            Some(this.copy(rolls = rolls :+ nb_pins, state = State.SPARE))
          } else if(rolls.head + nb_pins < 10){
            Some(this.copy(rolls = rolls :+ nb_pins))
          } else{
            None
          }
        }
      } else if(this.rolls.size == 2){
        if(this.state != State.NOTHING) {
          Some(this.copy(rolls = rolls :+ nb_pins))
        } else {
          None
        }
      } else None
    }
  }

  /**
    * Answer if another roll can be performed on this frame
    * @return True if the player can performed another roll inside this frame, False otherwise
    */
  def canRoll: Boolean = true
}
