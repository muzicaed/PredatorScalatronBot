package control

import utils.{MiniBot, View, XY}

/**
 * Main control for missile bot.
 * A smart bomb that focus on exploding when valuable
 * (causing high damage). Will self destruct in danger.
 * Low priority but collects food, and splits into two Missiles
 * if energy gets high.
 */
object MissileControl {

  /**
   * Apply
   */
  def apply(bot: MiniBot) {
    bot.status("Missile[" + bot.energy.toString + "]")
    if (SharedWeaponControl.shouldSelfDestruct(bot)) {
      SharedWeaponControl.selfDestruct(bot)
    } else if (!SharedWeaponControl.tryValuableExplosion(bot)) {

      val moveDirection = analyzeView(bot.view, XY.Zero)
      bot.move(moveDirection)

      if (bot.energy > 100) {
        SharedWeaponControl.fireMissile(bot)
      } else if (bot.view.countVisibleEnemies() == 0) {
        bot.set("type" -> "Hunter")
      } else {
        val warpDirection = analyzeView(bot.view, moveDirection)
        SharedControl.warpBotInDirection(bot, moveDirection, warpDirection)
      }
    }
  }

  /**
   * Moves the bot.
   */
  /*
  def move(bot: MiniBot) {
    val target = bot.inputAsXYOrElse("target", XY.Zero)
    val directionValue = analyzeView(bot.view)
    directionValue(target.toDirection45) += 100
    val lastMove = SharedControl.moveBotInDirection(bot, directionValue)
    bot.set("target" -> lastMove)
  }
  */

  /**
   * Analyze the view.
   */
  def analyzeView(view: View, offsetPos: XY) = {
    val directionValue = Array.ofDim[Double](8)

    for (i <- 0 until view.cells.length) {
      val cellRelPos = view.relPosFromIndexFromOffset(i, offsetPos)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = view.cells(i) match {
          case 'm' => if (stepDistance <= 1) -150 else 200 - stepDistance * 10 // enemy master
          case 's' => if (stepDistance <= 2) -150 else 180 - stepDistance * 10 // enemy slave
          case 'M' => -50 // my master
          case 'S' => if (stepDistance < 3) -5 else -50 // friendly slave
          case 'B' => if (stepDistance <= 5) 100 else 0 // good beast
          case 'P' => if (stepDistance <= 3) 80 else 0 // good plant
          case 'b' => if (stepDistance <= 3) -150 else 0 // bad beast
          case 'p' => if (stepDistance < 2) -100 else 0 // bad plant
          case 'W' => if (stepDistance < 3) -10000 else 0 // wall

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }

    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    XY.fromDirection45(bestDirection45)
  }
}
