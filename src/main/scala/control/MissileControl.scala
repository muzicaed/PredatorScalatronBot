package control

import utils.{MiniBot, View, XY}

/**
 * Main control for master bot.
 */
object MissileControl {
  /**
   * Apply
   */
  def apply(bot: MiniBot) {
    bot.view.offsetToNearest('m') match {
      case Some(delta: XY) =>
        // close enough to blow it up?
        if (delta.length <= 2) {
          bot.explode(3)
          return
        }
      case None =>
    }

    bot.view.offsetToNearest('s') match {
      case Some(delta: XY) =>
        // close enough to blow it up?
        if (delta.length <= 3) {
          bot.explode(6)
          return
        }
      case None =>
    }

    val target = bot.inputAsXYOrElse("target", XY.Zero)
    val directionValue = analyzeView(bot.view)
    directionValue(target.toDirection45) += 500
    SharedControl.moveBotInDirection(bot, directionValue)
  }

  /**
   * Analyze the view.
   */
  def analyzeView(view: View) = {
    val directionValue = Array.ofDim[Double](8)

    val cells = view.cells
    val cellCount = cells.length
    for (i <- 0 until cellCount) {
      val cellRelPos = view.relPosFromIndex(i)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = cells(i) match {
          case 'm' => // another master: not dangerous, but an obstacle
            (1500 - stepDistance * 10)

          case 's' => // another slave: potentially dangerous?
            (2000 - stepDistance * 10)

          case 'B' => // good beast: valuable, but runs away
            0.0

          case 'P' => // good plant: less valuable, but does not run
            0.0

          case 'b' => // bad beast: dangerous, but only if very close
            if (stepDistance < 3) -1500 else 0

          case 'p' => // bad plant: bad, but only if I step on it
            if (stepDistance < 2) -1000 else 0

          case 'W' => // wall: harmless, just don't walk into it
            if (stepDistance < 4) -2000 else 0

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    directionValue
  }
}
