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
    //bot.status("Missile[" + bot.energy.toString + "]")
    if (SharedWeaponControl.shouldSelfDestruct(bot)) {
      SharedWeaponControl.selfDestruct(bot)
    } else if (!SharedWeaponControl.tryValuableExplosion(bot)) {
      if (bot.energy > 500 || (bot.view.offsetToNearestEnemy().stepCount < 5 && bot.energy > 200)) {
        SharedWeaponControl.fireMissile(bot)
      } else if (bot.view.countVisibleEnemies() == 0) {
        bot.set("type" -> "Vampire")
      }

      move(bot)
    }
  }

  /**
   * Moves the bot.
   */
  def move(bot: MiniBot) {
    val target = bot.inputAsXYOrElse("target", XY.Zero)
    val directionValue = analyzeView(bot.view)
    directionValue(target.toDirection45) += 100
    val lastMove = SharedControl.moveBotInDirection(bot, directionValue)
    bot.set("target" -> lastMove)
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
          case 'm' => if (stepDistance <= 3) -150 else 150 - stepDistance * 10 // enemy master
          case 's' => if (stepDistance <= 5) -150 else 120 - stepDistance * 10 // enemy slave
          case 'M' => -50 // my master
          case 'S' => -50 // friendly slave
          case 'B' => if (stepDistance <= 5) 100 else 0 // good beast
          case 'P' => if (stepDistance <= 3) 80 else 0 // good plant
          case 'b' => if (stepDistance <= 3) -150 else 0 // bad beast
          case 'p' => if (stepDistance < 2) -100 else 0 // bad plant
          case 'W' => if (stepDistance < 2) -1000 else 0 // wall

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    directionValue
  }
}
