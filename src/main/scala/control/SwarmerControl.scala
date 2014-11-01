package control

import utils.{Bot, MiniBot, XY}

/**
 * Main control for swarmer bot.
 * Stays close to master and collects.
 * If danger converts to defence bot.
 */
object SwarmerControl {

  def apply(bot: MiniBot) {
    bot.status("Swarmer[" + bot.energy.toString + "]")
    if (SharedWeaponControl.shouldSelfDestruct(bot)) {
      SharedWeaponControl.selfDestruct(bot)
    } else {

      if (!handleDanger(bot)) {
        if (!SharedWeaponControl.tryValuableExplosion(bot)) {
          if (bot.offsetToMaster.stepCount > 10 || bot.energy > 275) {
            headHome(bot)
          } else {
            move(bot)
          }
        }
      }
    }

  }

  /**
   * Moves the bot.
   */
  def move(bot: MiniBot) {
    val target = bot.inputAsXYOrElse("target", XY.Zero)
    val directionValue = analyzeView(bot)
    directionValue(target.toDirection45) += 300
    val lastMove = SharedControl.moveBotInDirection(bot, directionValue)
    bot.set("target" -> lastMove)
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
            if (stepDistance < 6) -2000
            else 0

          case 's' => // enemy slave
            if (stepDistance < 7) -2000
            else 0

          case 'P' => // good plant
            if (stepDistance == 1) 100
            else if (stepDistance < 3) 40
            else (40 - stepDistance).max(0)

          case 'B' => // good beast
            if (stepDistance == 1) 80
            else if (stepDistance < 3) 30
            else (30 - stepDistance).max(0)

          case 'b' => // bad beast
            if (stepDistance < 2) -60
            else if (stepDistance < 5) -60 / stepDistance
            else 0

          case 'M' => // friendly master
            if (stepDistance > 6) 600 else 0

          case 'S' => -50 // friendly slave
          case 'p' => if (stepDistance < 3) -50 else 0 // bad plant
          case 'W' => if (stepDistance < 2) -10000 else 0 // wall
          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    directionValue
  }

  /**
   * If to dangerous, convert into a defence bot.
   */
  def handleDanger(bot: MiniBot): Boolean = {
    if (bot.view.countVisibleEnemies() > 2) {
      bot.set("type" -> "Vampire")
      true
    } else if (bot.offsetToMaster.stepCount > 13) {
      bot.set("type" -> "Missile")
      true
    }
    false
  }

  /**
   * Head home to master
   */
  def headHome(bot: MiniBot) {
    val homeDirection = bot.offsetToMaster.toDirection45
    val directionValue = analyzeView(bot)
    directionValue(homeDirection) += 80
    val lastMove = SharedControl.moveBotInDirection(bot, directionValue)
    bot.set("target" -> lastMove)
  }
}
