package control

import utils.{Bot, MiniBot}

/**
 * Main control for vampire bot.
 * The Vampire is a slightly more aggressive "mini master".
 * Collects food like master, but will use more fire power.
 * Will spawn more vampires.
 */
object VampireControl {

  def apply(bot: MiniBot) {
    //bot.status("Vamp [" + bot.energy.toString + "]")
    if (SharedWeaponControl.shouldSelfDestruct(bot)) {
      SharedWeaponControl.selfDestruct(bot)
    } else {

      if (!SharedWeaponControl.tryDropBomb(bot))
        if (!SharedWeaponControl.tryValuableExplosion(bot)) {
          val directionValue = analyzeView(bot)
          val moveDirection = SharedControl.moveBotInDirection(bot, directionValue)
          if (SharedControl.checkFireMissile(bot)) {
            SharedControl.fireMissile(bot)
          } else if (bot.energy > 1500) {
            SharedControl.spawnHunter(bot, moveDirection)
          }
        }
    }

    if (bot.energy < 200) {
      bot.set("type" -> "Hunter")
    }
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
            if (stepDistance < 10 || bot.energy < 1000) -200
            else 200 - stepDistance

          case 's' => // enemy slave
            if (stepDistance < 10 || bot.energy < 1000) -250
            else 20 - stepDistance

          case 'B' => // good beast
            if (stepDistance == 1) 100
            else if (stepDistance < 6) 80
            else (80 - stepDistance).max(0)

          case 'b' => // bad beast
            if (stepDistance < 2) -100
            else if (stepDistance < 5) -100 / stepDistance
            else 0

          case 'S' => -100 / stepDistance // friendly slave
          case 'M' => -500 // friendly master
          case 'P' => if (stepDistance < 3) 80 else 0 // good plant
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
