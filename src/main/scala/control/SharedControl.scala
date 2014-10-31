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

  /**
   * Fire a missile.
   */
  def fireMissile(bot: Bot): Unit = {
    var fireRate = 4
    var power = 100
    if (bot.energy > 2000) {
      fireRate = 3
      power = 110
    } else if (bot.energy > 4000) {
      fireRate = 3
      power = 120
    } else if (bot.energy > 10000) {
      fireRate = 2
      power = 130
    }

    val relPos = bot.view.offsetToNearestEnemy()
    bot.spawn(relPos.signum, "type" -> "Missile", "target" -> relPos, "energy" -> power)
    bot.set("missileDelay" -> (bot.time + fireRate))
  }

  /**
   * Checks if now is a good time to fire
   * a missile.
   */
  def checkFireMissile(bot: Bot): Boolean = {
    (bot.view.countType('m') > 0 || bot.view.countType('s') > 0 || bot.view.countType('b') > 4) &&
      bot.time > bot.inputAsIntOrElse("missileDelay", -1) &&
      bot.energy > 300
  }
}