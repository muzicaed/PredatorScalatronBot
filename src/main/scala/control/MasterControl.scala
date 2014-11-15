package control

import utils.{Bot, MiniBot, XY, Const}

/**
 * Main control for master bot.
 */
object MasterControl {

  def apply(bot: MiniBot) {
    bot.status("-:[ muzicaed ]:-")
    val moveDirection = analyzeView(bot)
    bot.move(moveDirection)

    if (!SharedWeaponControl.handleDanger(bot)) {
      if (checkEntitySpawn(bot)) {
        if (bot.energy > 5000) {
          SharedWeaponControl.spawnVampire(bot, moveDirection.negate)
        } else {
          SharedWeaponControl.spawnHunter(bot, moveDirection.negate)
        }
      } else if (SharedWeaponControl.checkFireMissile(bot)) {
        SharedWeaponControl.fireMissile(bot)
      } else {
        launchSwarmer(bot)
      }
    }
  }

  /**
   * Check if now is a good time to spawn Hunter or Vampire
   */
  def checkEntitySpawn(bot: Bot): Boolean = {
    val hunterTime = bot.inputAsIntOrElse("hunterTimeCount", -1)
    if (bot.energy > 5000 && bot.time > hunterTime && bot.slaves < Const.SpawnLimit) {
      bot.set("hunterTimeCount" -> (bot.time + 0))
      return true
    }
    false
  }

  /**
   * Launch a swarmer.
   */
  def launchSwarmer(bot: Bot) = {
    if ((bot.time > bot.inputAsIntOrElse("swarmerDelay", -1)) && bot.slaves < Const.SpawnLimit) {
      if (bot.energy > 500 && countSwarmers(bot) <= 4 && bot.view.countType('m') == 0 && bot.view.countType('s') <= 2) {
        val rnd = new scala.util.Random
        val spawnDirection = XY.fromDirection45(rnd.nextInt(8))
        bot.spawn(spawnDirection.signum, "type" -> "Swarmer", "target" -> spawnDirection.toDirection45, "energy" -> 104)
        bot.set("swarmerDelay" -> (bot.time + 1))
      }
    }
  }

  /**
   * Tries to count the swarmers.
   */
  def countSwarmers(bot: Bot): Int = {
    var count = 0
    val slaves = bot.view.getRelPosForType('S')

    if (slaves.nonEmpty) {
      slaves.foreach {
        case (typeChar, relPos) =>
          if (relPos.distanceTo(XY.Zero) < 8) {
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

    var i = 0
    while (i < bot.view.cells.length) {
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
            else (80 - stepDistance).max(10)

          case 'b' => // bad beast
            if (stepDistance < 3) -500
            else -100 / stepDistance

          case 'S' => 5 // friendly slave
          case 'P' => if (stepDistance < 3) 120 else (80 - stepDistance).max(5) // good plant
          case 'p' => if (stepDistance < 3) -80 else 0 // bad plant
          case 'W' => if (stepDistance < 2) -10000 else -20 / stepDistance // wall
          case _ => 1 / stepDistance
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
      i += 1
    }
    SharedControl.convertDirectionValueIntoMove(bot, directionValue)
  }
}
