package bowling

import org.scalatest.{FunSpec, Matchers}

import scala.util.Random

class BowlingTest extends FunSpec with Matchers {
  describe("Player") {
    describe("When new game") {
      it("should have a score of 0") {
        assert(Player(random = new Random()).score == 0)
      }
      it("should start with the first frame empty") {
        val player = Player(random = new Random())
        assert(player.frames.size == 1)
        assert(player.current_frame == 1)
        assert(player.frames.head.rolls.isEmpty)
      }
    }
    describe("When rolling") {
      it("Should roll between 0 and 10") {
        val roll = Player(random = new Random()).roll()
        assert(roll.isDefined)
        assert(0 <= roll.get && roll.get <= 10)
      }
      it("Should return a None value when trying to roll 3 times during one of the 9 frames") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(3).get
        val player3 = player2.updateScore(3).get
        assert(player3.roll().isEmpty)
      }
      it("Should update the score with a roll between 0 and 10") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(7)
        assert(player2.isDefined)
        assert(player2.get.score == 7)
      }
      it("Should return an option player if we update the score with a total higher than 10 for a frame for the first 9" +
        " frames") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(7).get
        val player3 = player2.updateScore(5)
        assert(player3.isEmpty)
      }
      it("Should return a player if we update the score with a total lower than 10 for a frame for the first 9 frames") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(7).get
        val player3 = player2.updateScore(2)
        assert(player3.isDefined)
        assert(player3.get.score == 9)
      }
      it("Should return a player with the frame state to strike if we roll 10 the first roll of a frame") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(10).get
        assert(player2.frames.head.state == State.STRIKE)
        assert(player2.score == 10)
      }
      it("Should return a player with the bonus updated to (1,1) if we roll 10 the first roll of a frame") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(10).get
        assert(player2.bonusRoll == List(1, 1))
      }
      it("Should add an int to the rolls list of the current frame") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(5).get
        assert(player2.frames.head.rolls.nonEmpty)
      }
      it("Should add 2 int to the rolls list of the current frame when rolling 2 times") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(5).get
        assert(player2.frames.head.rolls.nonEmpty)
        val player3 = player2.updateScore(5).get
        assert(player3.frames.head.rolls.size == 2)
      }
      it("Should return a player with the bonus updated to (1,0) if we roll 10 the first roll of a frame and anything " +
        "else the first roll of the second frame") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(10).get
        val player3 = player2.nextFrame().get
        val player4 = player3.updateScore(5).get
        assert(player4.bonusRoll == List(1, 0))
      }
      it("Should return a player with score updated (with multiplicateur of the  strike) if we roll 10 the first roll " +
        "of a frame and anything else on the first roll of the second frame (except 10)") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(10).get
        val player3 = player2.nextFrame().get
        val player4 = player3.updateScore(5).get
        assert(player4.score == 20)
      }
      it("Should return a player with the frame state to spare if we roll 10 in the first 2 rolls of a frame") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(5).get
        val player3 = player2.updateScore(5).get
        assert(player3.frames.head.state == State.SPARE)
      }
      it("Should return a player with the bonus updated to (1,0) if we roll 10 in the first 2 rolls of a frame") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(5).get
        val player3 = player2.updateScore(5).get
        assert(player3.bonusRoll == List(1, 0))
      }
      it("Should add the next frame when the current one is finished") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(10).get
        val player3 = player2.nextFrame()
        assert(player3.isDefined)
      }
      it("Should return a player with the bonus updated to (0,0) if we roll 10 the first 2 rolls of a frame and anything " +
        "else on the first roll of the second frame (except 10)") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(5).get
        val player3 = player2.updateScore(5).get
        val player4 = player3.nextFrame().get
        val player5 = player4.updateScore(5).get
        assert(player5.bonusRoll == List(0, 0))
      }
      it("Should return a player with score updated (with multiplier of the square) if we roll 10 in the first 2 " +
        "rolls of a frame and anything else the first roll of the second frame") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(5).get
        val player3 = player2.updateScore(5).get
        val player4 = player3.nextFrame().get
        val player5 = player4.updateScore(5).get
        assert(player5.score == 20)
      }
      it("Should not be able to roll 3 times in the same frame for the first 9 frames") {
        val player = Player(random = new Random())
        val player2 = player.updateScore(3).get
        val player3 = player2.updateScore(3).get
        val player4 = player3.updateScore(3)
        assert(player4.isEmpty)
      }
      it("Should not be able to roll 3 times in the 10th frame if the sum of the 2 first rolls are note equal to 10") {
        var player: Player = Player(random = new Random())
        for (_ <- 1 to 9) {
          player = player.updateScore(10).get
          player = player.nextFrame().get
        }
        player = player.updateScore(5).get
        assert(player.canRollOnTheCurrentFrame)
        val player2 = player.updateScore(3)
        assert(player2.isDefined)
        assert(!player2.get.canRollOnTheCurrentFrame)
        val player3 = player2.get.updateScore(5)
        assert(player3.isEmpty)
      }
      it("Should be able to roll 3 times in the 10th frame if a strike was done the first roll") {
        var player: Player = Player(random = new Random())
        for (_ <- 1 to 9) {
          player = player.updateScore(10).get
          player = player.nextFrame().get
        }
        player = player.updateScore(10).get
        assert(player.canRollOnTheCurrentFrame)
        val player2 = player.updateScore(5)
        assert(player2.isDefined)
        assert(player2.get.canRollOnTheCurrentFrame)
        val player3 = player2.get.updateScore(5)
        assert(player3.isDefined)
        assert(!player3.get.canRollOnTheCurrentFrame)
        val player4 = player3.get.updateScore(5)
        assert(player4.isEmpty)
      }
      it("Should be able to roll 3 times in the 10th frame if a spare was done the first 2 rolls") {
        var player: Player = Player(random = new Random())
        for (_ <- 1 to 9) {
          player = player.updateScore(10).get
          player = player.nextFrame().get
        }
        val player2 = player.updateScore(5)
        assert(player2.isDefined)
        assert(player2.get.canRollOnTheCurrentFrame)
        val player3 = player2.get.updateScore(5)
        assert(player3.isDefined)
        assert(player3.get.canRollOnTheCurrentFrame)
        val player4 = player3.get.updateScore(5)
        assert(player4.isDefined)
        assert(!player4.get.canRollOnTheCurrentFrame)
        val player5 = player4.get.updateScore(5)
        assert(player5.isEmpty)
      }
      it("Should give 300 score if we roll all the strikes") {
        var player: Player = Player(random = new Random())
        for (_ <- 1 to 9) {
          player = player.updateScore(10).get
          player = player.nextFrame().get
        }
        player = player.updateScore(10).get
        player = player.updateScore(10).get
        player = player.updateScore(10).get
        assert(player.score == 300)
      }
    }
    describe("At the end") {
      it("Should reset the player correctly") {
        var player: Player = Player(username = "Test", random = new Random())
        player = player.updateScore(10).get
        val resetPlayer = player.reset()
        assert(resetPlayer.username == "Test")
        assert(resetPlayer.frames.size == 1)
        assert(resetPlayer.frames(0) == Frame(nb_frame = 1))
        assert(resetPlayer.score == 0)
      }
    }
  }
}