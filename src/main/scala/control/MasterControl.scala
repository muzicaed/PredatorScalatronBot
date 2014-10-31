package control

import utils.{Bot, XY}

/**
 * Main control for master bot.
 */
object MasterControl {

  def apply(bot: Bot) {
    val directionValue = analyzeView(bot)
    val moveDirection = SharedControl.moveBotInDirection(bot, directionValue)

    if (checkVampireSpawn(bot)) {
      print("Spawn vampire!")
      spawnVampire(bot, moveDirection)
    } else if (SharedControl.checkFireMissile(bot)) {
      SharedControl.fireMissile(bot)
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
            else 50 - stepDistance

          case 's' => // enemy slave
            enemies +:= cellRelPos
            if (stepDistance < 10 || bot.energy < 10000) -100
            else 80 - stepDistance

          case 'B' => // good beast
            if (stepDistance == 1) 100
            else if (stepDistance < 6) 80
            else (80 - stepDistance).max(0)

          case 'b' => // bad beast
            if (stepDistance < 2) -100
            else if (stepDistance < 5) -100 / stepDistance
            else 0

          case 'S' => -500 // friendly slave
          case 'P' => if (stepDistance < 3) 80 else 0 // good plant
          case 'p' => if (stepDistance < 3) -80 else 0 // bad plant
          case 'W' => if (stepDistance < 3) -50 else 0 // wall
          case '?' => -20 / stepDistance // Unknown (Behind wall)
          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    directionValue
  }

  /**
   * Checks if now is a good time to spawn a Vampire.
   */
  def checkVampireSpawn(bot: Bot): Boolean = {
    val vampireTimeCount = bot.inputAsIntOrElse("vampireTimeCount", 0)
    bot.set("vampireTimeCount" -> (vampireTimeCount + 1))
    (bot.energy > 15000 && vampireTimeCount > 30) || (bot.energy > 5000 && vampireTimeCount > 70)
  }

  /**
   * Spawn a Vampire
   */
  def spawnVampire(bot: Bot, moveDirection: XY): Unit = {
    var energyTransfer = bot.energy * 0.05
    if (bot.energy > 8000) energyTransfer = bot.energy * 0.15
    else if (bot.energy > 30000) energyTransfer = bot.energy * 0.20

    bot.spawn(moveDirection.negate, "type" -> "Vampire", "energy" -> energyTransfer.toInt)
    bot.set("vampireTimeCount" -> 0)
    bot.say("Rise from the dead!")
  }
}
