package control

import utils.{Bot, MiniBot, XY}

/**
 * Shared control functions
 */
object SharedControl {

  /**
   * Moves bot in direction and stores as last direction.
   */
  def moveBotInDirection(bot: MiniBot, directionValue: Array[Double]) = {


    // If Mini-Bot and apocalypse closing in, head home!
    if (bot.generation > 0 && bot.apocalypse < 90) {
      val directionXY = bot.offsetToMaster
      directionValue(directionXY.toDirection45) += 10000
    }

    // determine movement direction
    directionValue(lastDirection) += 70 // try to break ties by favoring the last direction
    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    val direction = XY.fromDirection45(bestDirection45)

    bot.move(direction)
    bot.set("lastDirection" -> bestDirection45)
    direction
  }

  def warpBotInDirection(bot: MiniBot, directionValue: Array[Double]): Unit = {

  }
}