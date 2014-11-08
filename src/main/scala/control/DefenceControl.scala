package control

import utils.{Const, MiniBot, XY}

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
    if (bot.energy > 0) bot.status("Defence")
    val moveDirection = analyzeView(bot, XY.Zero)
    bot.move(moveDirection)

    if (bot.view.countVisibleEnemies() > 0 && bot.energy > 100) {
      bot.spawn(moveDirection, "type" -> "Defence", "target" -> moveDirection.toDirection45, "energy" -> bot.energy / 2)
    } else if (bot.view.countType('s') == 0) {
      if (bot.slaves < Const.SpawnLimit) {
        bot.set("type" -> "Hunter")
      } else {
        SharedWeaponControl.selfDestruct(bot)
      }
    } else {
      val warpDirection = analyzeView(bot, moveDirection.signum)
      SharedControl.warpBotInDirection(bot, moveDirection, warpDirection)
    }
  }

  /**
   * Analyze the view.
   */
  def analyzeView(bot: MiniBot, offsetPos: XY) = {
    val directionValue = Array.ofDim[Double](8)

    var i = 0
    while (i < bot.view.cells.length) {
      val cellRelPos = bot.view.relPosFromIndexFromOffset(i, offsetPos)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = bot.view.cells(i) match {
          case 'm' => if (stepDistance <= 3) -150 else 0 // enemy master
          case 's' => 500 / stepDistance
          case 'M' => -50 / stepDistance // my master
          case 'S' => if (stepDistance < 3) -5 else -50 / stepDistance // friendly slave
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
