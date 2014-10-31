package control

import utils.{Bot, XY}

/**
 * Main control for vampire bot.
 */
object VampireControl {
  def apply(bot: Bot) {
    val directionValue = analyzeView(bot)
    val missileDelay = bot.inputAsIntOrElse("missileDelay", -1)
    val vampireTimeCount = bot.inputAsIntOrElse("vampireCount", 0)
    bot.set("vampireTimeCount" -> (vampireTimeCount + 1))

    val moveDirection = SharedControl.moveBotInDirection(bot, directionValue)


    if ((bot.energy > 15000 && vampireTimeCount > 30) || (bot.energy > 5000 && vampireTimeCount > 70)) {
      var energyTransfer = bot.energy * 0.05
      if (bot.energy > 15000) energyTransfer = bot.energy * 0.10
      else if (bot.energy > 50000) energyTransfer = bot.energy * 0.15

      bot.spawn(moveDirection.negate, "type" -> "Vampire", "energy" -> energyTransfer.toInt)
      bot.set("vampireTimeCount" -> 0)
      bot.say("Rise from the dead!")
    } else if ((bot.view.countType('m') > 0 || bot.view.countType('s') > 0 || bot.view.countType('b') > 4) && bot.time > missileDelay && bot.energy > 1000) {
      var fireRate = 6
      var power = 100
      if (bot.energy > 2000) {
        fireRate = 4
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
  }

  /**
   * Analyze the view, building a map of attractiveness for the 45-degree directions and
   * recording other relevant data, such as the nearest elements of various kinds.
   */
  def analyzeView(bot: Bot) = {
    val directionValue = Array.ofDim[Double](8)
    var enemies = Array[XY]()

    for (i <- 0 until bot.view.cells.length) {
      val cellRelPos = bot.view.relPosFromIndex(i)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = bot.view.cells(i) match {
          case 'm' => // another master
            enemies +:= cellRelPos
            if (stepDistance < 10 || bot.energy < 5000) -100
            else 100 - stepDistance

          case 's' => // enemy slave
            enemies +:= cellRelPos
            if (stepDistance < 10 || bot.energy < 10000) -100
            else 150 - stepDistance

          case 'S' => // friendly slave
            -500

          case 'B' => // good beast
            if (stepDistance == 1) 60
            else if (stepDistance < 7) 30
            else (30 - stepDistance).max(0)

          case 'P' => // good plant
            if (stepDistance < 4) 50 else 0

          case 'b' => // bad beast
            if (stepDistance < 2) -100
            else if (stepDistance < 5) -100 / stepDistance
            else 0

          // bad plant
          case 'p' => if (stepDistance < 3) -80 else 0

          // wall
          case 'W' => if (stepDistance < 3) -50 else 0

          // Unknown (Behind wall)
          case '?' => -20 / stepDistance

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    directionValue
  }
}
