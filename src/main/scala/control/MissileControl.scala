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
    if (bot.energy > 0) bot.status("Missile[" + bot.energy.toString + "]")
    if (SharedWeaponControl.shouldSelfDestruct(bot)) {
      SharedWeaponControl.selfDestruct(bot)
    } else if (!SharedWeaponControl.tryValuableExplosion(bot)) {

      val moveDirection = analyzeView(bot, XY.Zero)
      bot.move(moveDirection)

      if (bot.view.countVisibleEnemies() == 0) {
        bot.set("type" -> "Hunter")
      } else if (bot.energy > 100) {
        SharedWeaponControl.fireMissile(bot)

      } else {
        val warpDirection = analyzeView(bot, moveDirection)
        SharedControl.warpBotInDirection(bot, moveDirection, warpDirection)
      }
    }
  }

  /**
   * Analyze the view.
   */
  def analyzeView(bot: MiniBot, offsetPos: XY) = {
    val directionValue = Array.ofDim[Double](8)

    for (i <- 0 until bot.view.cells.length) {
      val cellRelPos = bot.view.relPosFromIndexFromOffset(i, offsetPos)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = bot.view.cells(i) match {
          case 'm' => if (stepDistance <= 1) -150 else 800 / stepDistance // enemy master
          case 's' => if (stepDistance <= 2) -150 else 500 / stepDistance // enemy slave
          case 'M' => -50 / stepDistance // my master
          case 'S' => if (stepDistance < 3) -5 else -50 / stepDistance // friendly slave
          case 'B' => if (stepDistance <= 5) 100 else 0 // good beast
          case 'P' => if (stepDistance <= 3) 80 else 0 // good plant
          case 'b' => 100 / stepDistance // bad beast
          case 'p' => if (stepDistance < 3) -100 else 0 // bad plant
          case 'W' => if (stepDistance < 2) -10000 else -20 / stepDistance // wall
          case _ => 1 / stepDistance
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }

    SharedControl.convertDirectionValueIntoMove(bot, directionValue)
  }
}
