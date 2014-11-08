package control

import utils.{XY, Bot, MiniBot}

/**
 * Main control for vampire bot.
 * The Vampire is a slightly more aggressive "mini master".
 * Collects food like master, but will use more fire power.
 * Will spawn more vampires.
 */
object VampireControl {

  def apply(bot: MiniBot) {
    //if (bot.energy > 0) bot.status("Vamp [" + bot.energy.toString + "]")
    if (SharedWeaponControl.shouldSelfDestruct(bot)) {
      SharedWeaponControl.selfDestruct(bot)
    } else {
      val moveDirection = analyzeView(bot, XY.Zero)
      bot.move(moveDirection)

      if (!SharedWeaponControl.tryDropBomb(bot))
        if (!SharedWeaponControl.tryValuableExplosion(bot)) {
          if (SharedWeaponControl.checkFireMissile(bot)) {
            SharedWeaponControl.fireMissile(bot)
          } else if (bot.energy > 3000 && bot.slaves < SharedControl.SpawnLimit) {
            SharedWeaponControl.spawnVampire(bot, moveDirection.negate)
          } else {
            val warpDirection = analyzeView(bot, moveDirection.signum)
            SharedControl.warpBotInDirection(bot, moveDirection, warpDirection)
          }
        }
    }

    if (bot.energy < 150) {
      bot.set("type" -> "Hunter")
    }
  }

  /**
   * Analyze the view, building a map of attractiveness for the 45-degree directions and
   * recording other relevant data, such as the nearest elements of various kinds.
   */
  def analyzeView(bot: Bot, offsetPos: XY) = {
    val directionValue = Array.ofDim[Double](8)

    var i = 0
    while (i < bot.view.cells.length) {
      val cellRelPos = bot.view.relPosFromIndexFromOffset(i, offsetPos)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = bot.view.cells(i) match {
          case 'm' => // another master
            if (stepDistance < 10 || bot.energy < 1000) -200
            else 200 / stepDistance

          case 's' => // enemy slave
            if (stepDistance < 10 || bot.energy < 1000) -250
            else 100 / stepDistance

          case 'B' => // good beast
            if (stepDistance == 1) 100
            else if (stepDistance < 6) 80
            else (80 - stepDistance).max(0)

          case 'b' => if (stepDistance <= 2) -150 else 90 / stepDistance // bad beast

          case 'S' => -100 / stepDistance // friendly slave
          case 'M' => -500 // friendly master
          case 'P' => if (stepDistance < 3) 80 else 0 // good plant
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
