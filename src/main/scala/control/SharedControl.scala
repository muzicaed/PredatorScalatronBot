package control

import utils.{Bot, XY}

/**
 * Shared control functions
 */
object SharedControl {
  /**
   * Moves bot in direction and stores as last direction.
   */
  def moveBotInDirection(bot: Bot, directionValue: Array[Double]) = {
    val lastDirection = bot.inputAsIntOrElse("lastDirection", 0)

    // determine movement direction
    directionValue(lastDirection) += 10 // try to break ties by favoring the last direction
    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    val direction = XY.fromDirection45(bestDirection45)
    bot.move(direction)
    bot.set("lastDirection" -> bestDirection45)
    direction
  }
}