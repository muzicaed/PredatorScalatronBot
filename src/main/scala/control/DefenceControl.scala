package control

import utils.{MiniBot, View, XY}

/**
 * Main control for defence bot.
 * Master will launch if in danger.
 * Linear targets near-by enemy slaves and collides.
 */
object DefenceControl {

  /**
   * Apply
   */
  def apply(bot: MiniBot) {
    //bot.status("Defence")
    val target = bot.inputAsXYOrElse("target", XY.Zero)
    val directionValue = analyzeView(bot.view)
    directionValue(target.toDirection45) += 100
    val lastMove = SharedControl.moveBotInDirection(bot, directionValue)
    bot.set("target" -> lastMove)

    if (bot.view.countVisibleEnemies() > 0 && bot.energy > 100) {
      bot.spawn(lastMove.signum, "type" -> "Defence", "target" -> target, "energy" -> bot.energy / 2)
    } else if (bot.view.countVisibleEnemies() == 0) {
      bot.set("type" -> "Vampire")
    }
  }

  /**
   * Analyze the view.
   */
  def analyzeView(view: View) = {
    val directionValue = Array.ofDim[Double](8)

    for (i <- 0 until view.cells.length) {
      val cellRelPos = view.relPosFromIndex(i)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = view.cells(i) match {
          case 'm' => if (stepDistance <= 3) -150 else 0 // enemy master
          case 's' => 1000
          case 'M' => -50 // my master
          case 'S' => -50 // friendly slave
          case 'W' => if (stepDistance < 2) -200 else 0 // wall

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    directionValue
  }
}
