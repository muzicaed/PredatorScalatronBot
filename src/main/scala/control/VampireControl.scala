package control

import utils.{Const, MiniBot, XY}

/**
 * Main control for vampire bot.
 * The Vampire is a slightly more aggressive "mini master".
 * Collects food like master, but will use more fire power.
 * Will spawn more vampires.
 */
object VampireControl {

  def apply(bot: MiniBot) {
    if (Const.DEBUG && bot.energy > 0) bot.status("Vamp [" + bot.energy.toString + "]")

    if (bot.time < 100 && bot.energy > 200) {
      val moveDirection = analyzeView(bot, XY.Zero, false)
      bot.move(moveDirection)
      SharedWeaponControl.spawnVampire(bot, moveDirection.negate)
    } else {
      if (SharedWeaponControl.shouldSelfDestruct(bot)) {
        SharedWeaponControl.selfDestruct(bot)
      } else {
        var headHome = false
        if ((bot.energy > 5000 && bot.offsetToMaster.stepCount <= 15) || bot.apocalypse < 150) {
          headHome = true
        }
        val moveDirection = analyzeView(bot, XY.Zero, headHome)
        bot.move(moveDirection)

        if (!SharedWeaponControl.tryDropBomb(bot)) {
          if (!SharedWeaponControl.tryValuableExplosion(bot)) {
            if (SharedWeaponControl.checkFireMissile(bot)) {
              SharedWeaponControl.fireMissile(bot)
            }
            else if (!headHome && bot.energy > 300 && bot.slaves < Const.LOWER_SPAWN_LIMIT && bot.view.countType('S') < 2) {
              SharedWeaponControl.spawnVampire(bot, moveDirection.negate)
            }
          }
        }
      }
    }

    if (bot.energy < 150) {
      //bot.set("type" -> SlaveType.HUNTER)
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
            if (stepDistance < 7 || bot.energy < 400) -200
            else 200 / stepDistance

          case 's' => // enemy slave
            if (stepDistance < 7 || bot.energy < 400) -250
            else 100 / stepDistance

          case 'B' => // good beast
            if (stepDistance == 1) 100
            else if (stepDistance < 6) 80
            else (80 - stepDistance).max(0)

          case 'b' => if (stepDistance < 2) -1000 else 110 / stepDistance // bad beast

          case 'S' => if (stepDistance < 2) -1000 else -200 / stepDistance // friendly slave
          case 'M' => if (headHome) 200 / stepDistance else -200 / stepDistance // friendly master
          case 'P' => if (stepDistance < 3) 80 else 0 // good plant
          case 'p' => if (stepDistance < 3) -80 else 0 // bad plant
          case 'W' => if (stepDistance < 2) -10000 else -20 / stepDistance // wall
          case '_' => 1 / stepDistance
          case _ => 0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
      i += 1
    }

    if (headHome) {
      directionValue(bot.offsetToMaster.signum.toDirection45) += 500
    }
    SharedControl.convertDirectionValueIntoMove(bot, directionValue)
  }
}
