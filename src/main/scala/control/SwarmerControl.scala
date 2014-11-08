package control

import utils.{Const, MiniBot, XY}

/**
 * Main control for swarmer bot.
 * Stays close to master and collects.
 * If danger converts to defence bot.
 */
object SwarmerControl {

  def apply(bot: MiniBot) {
    if (bot.energy > 0) bot.status("Swarmer[" + bot.energy.toString + "]")

    if (SharedWeaponControl.shouldSelfDestruct(bot)) {
      SharedWeaponControl.selfDestruct(bot)
    } else {
      move(bot)

      if (!handleDanger(bot)) {
        if (!SharedWeaponControl.tryValuableExplosion(bot)) {
          SharedWeaponControl.tryDropBomb(bot)
        }
      }
    }
  }

  /**
   * Moves the bot. Decides if it is time to
   * head home.
   */
  def move(bot: MiniBot): Unit = {
    var headHome = false
    if (bot.offsetToMaster.stepCount > 10 || bot.energy > 275) {
      headHome = true
    }
    val moveDirection = analyzeView(bot, XY.Zero, headHome)
    bot.move(moveDirection)
    if (bot.slaves < Const.SpawnLimit) {
      val warpDirection = analyzeView(bot, moveDirection.signum, headHome)
      SharedControl.warpBotInDirection(bot, moveDirection, warpDirection)
    }
  }

  /**
   * Analyze the view, building a map of attractiveness for the 45-degree directions and
   * recording other relevant data, such as the nearest elements of various kinds.
   */
  def analyzeView(bot: MiniBot, offsetPos: XY, headHome: Boolean) = {
    val directionValue = Array.ofDim[Double](8)
    var i = 0
    while (i < bot.view.cells.length) {
      val cellRelPos = bot.view.relPosFromIndexFromOffset(i, offsetPos)
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
            if (stepDistance > 7) 600 else 0

          case 'S' => if (stepDistance == 0) 0 else -100 / stepDistance // friendly slave
          case 'p' => if (stepDistance < 3) -100 else 0 // bad plant
          case 'W' => if (stepDistance < 2) -10000 else -20 / stepDistance // wall
          case _ => 1 / stepDistance
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
      i += 1
    }

    if (headHome) {
      directionValue(bot.offsetToMaster.toDirection45) += 200
    }

    SharedControl.convertDirectionValueIntoMove(bot, directionValue)
  }

  /**
   * If to dangerous, convert into a missile or vampire
   */
  def handleDanger(bot: MiniBot): Boolean = {
    if (bot.view.countType('s') > 0 && bot.view.countType('m') == 0) {
      bot.set("type" -> "Defence")
      true
    } else if (bot.view.countType('m') > 0) {
      bot.set("type" -> "Missile")
      true
    }
    else if (bot.offsetToMaster.stepCount > 13 && bot.view.countType('s') == 0 && bot.slaves < Const.SpawnLimit) {
      bot.set("type" -> "Hunter")
      true
    }
    false
  }
}
