package control

import utils.{Bot, MiniBot, XY}

/**
 * Main control for master bot.
 */
object MasterControl {

  def apply(bot: MiniBot) {
    bot.status("-[::muzicaed::]-")
    val directionValue = analyzeView(bot)
    val rnd = new scala.util.Random
    var spawnDirection = XY.fromDirection45(rnd.nextInt(8))
    SharedControl.moveBotInDirection(bot, directionValue)
    spawnDirection = SharedControl.moveBotInDirection(bot, directionValue)
      if (bot.energy < 2000 || bot.view.countType('W') > 8) {
    }

    if (!SharedWeaponControl.handleDanger(bot)) {
      if (checkHunterSpawn(bot)) {
        if (bot.time > 200 && bot.energy > 2000) {
          SharedWeaponControl.spawnVampire(bot, spawnDirection)
        } else {
          SharedWeaponControl.spawnHunter(bot, spawnDirection)
        }

      } else if (SharedWeaponControl.checkFireMissile(bot)) {
        SharedWeaponControl.fireMissile(bot)
      } else {
        launchSwarmer(bot, spawnDirection)
      }
    }
  }

  /**
   * Check if now is a good time to spawn Hunter
   */
  def checkHunterSpawn(bot: Bot): Boolean = {
    val hunterTime = bot.inputAsIntOrElse("hunterTimeCount", -1)
    if (bot.energy > 1000 && bot.time > hunterTime) {
      bot.set("hunterTimeCount" -> (bot.time + 5))
      true
    }
    false
  }

  /**
   * Launch a swarmer.
   */
  def launchSwarmer(bot: Bot, moveDirection: XY) = {
    if ((bot.time > bot.inputAsIntOrElse("swarmerDelay", -1)) || bot.time < 10) {
      if (bot.energy > 500 && countSwarmers(bot) <= 5 && bot.view.countType('m') == 0 && bot.view.countType('s') <= 3) {
        bot.spawn(moveDirection.signum, "type" -> "Swarmer", "target" -> moveDirection)
        bot.set("swarmerDelay" -> (bot.time + 4))
      }
    }
  }

  /**
   * Tries to count the swarmers.
   */
  def countSwarmers(bot: Bot): Int = {
    var count = 0
    val slaves = bot.view.getRelPosForType('s')

    if (slaves.nonEmpty) {
      slaves.foreach {
        case (typeChar, relPos) =>
          if (relPos.stepsTo(XY.Zero) < 8) {
            count += 1
          }
      }
    }

    count
  }

  /**
   * Analyze the view, building a map of attractiveness for the 45-degree directions and
   * recording other relevant data, such as the nearest elements of various kinds.
   */
  def analyzeView(bot: Bot) = {
    val directionValue = Array.ofDim[Double](8)

    for (i <- 0 until bot.view.cells.length) {
      val cellRelPos = bot.view.relPosFromIndex(i)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = bot.view.cells(i) match {
          case 'm' => // another master
            if (stepDistance < 10 || bot.energy < 5000) -100
            else 50 - stepDistance

          case 's' => // enemy slave
            if (stepDistance < 10 || bot.energy < 10000) -100
            else 80 - stepDistance

          case 'B' => // good beast
            if (stepDistance == 1) 150
            else if (stepDistance < 6) 100
            else (80 - stepDistance).max(0)

          case 'b' => // bad beast
            if (stepDistance < 4) -100
            else if (stepDistance < 5) -100 / stepDistance
            else 0

          case 'S' => 0 // friendly slave
          case 'P' => if (stepDistance < 3) 120 else 0 // good plant
          case 'p' => if (stepDistance < 3) -80 else 0 // bad plant
          case 'W' => if (stepDistance < 2) -10000 else 0 // wall
          case '?' => -10 / stepDistance // Unknown (Behind wall)
          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    directionValue
  }
}
