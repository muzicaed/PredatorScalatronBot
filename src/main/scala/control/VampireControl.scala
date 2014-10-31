package control

import utils.{Bot, XY}

/**
 * Main control for vampire bot.
 */
object VampireControl {
  def apply(bot: Bot) {
    val directionValue = analyzeView(bot)
    SharedControl.moveBotInDirection(bot, directionValue)
    if (SharedControl.checkFireMissile(bot)) {
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
            else 100 - stepDistance

          case 's' => // enemy slave
            enemies +:= cellRelPos
            if (stepDistance < 10 || bot.energy < 10000) -100
            else 150 - stepDistance

          case 'B' => // good beast
            if (stepDistance == 1) 60
            else if (stepDistance < 7) 30
            else (30 - stepDistance).max(0)

          case 'b' => // bad beast
            if (stepDistance < 2) -100
            else if (stepDistance < 5) -100 / stepDistance
            else 0

          case 'P' => if (stepDistance < 4) 50 else 0 // good plant
          case 'S' => -500 // friendly slave
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
}
