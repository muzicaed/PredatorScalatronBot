package control

import utils._

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
    if (Const.DEBUG && bot.energy > 0) bot.status("Defence")
    val moveDirection = analyzeView(bot, XY.Zero)
    bot.move(moveDirection)

    if (bot.view.countVisibleEnemies() > 0 && bot.energy > 100) {
      bot.spawn(moveDirection, "type" -> SlaveType.DEFENCE, "target" -> moveDirection.toDirection45, "energy" -> bot.energy / 2)
    } else if (bot.view.countType('s') == 0) {
      if (bot.slaves < Const.LOWER_SPAWN_LIMIT) {
        bot.set("type" -> SlaveType.VAMPIRE)
      } else {
        SharedWeaponControl.selfDestruct(bot)
      }
    } else {
      val warpDirection = analyzeView(bot, moveDirection.signum)
      SharedControl.warpBotInDirection(bot, moveDirection, warpDirection)
    }
  }

  /**
   * Analyze the view and most valuable choose direction.
   */
  def analyzeView(bot: MiniBot, offsetPos: XY) = {
    val directionValue = Array.ofDim[Double](8)
    if (bot.time % 2 == 0) {
      var i = 0
      while (i < bot.view.cells.length) {
        val cellRelPos = bot.view.relPosFromIndexFromOffset(i, offsetPos)
        if (cellRelPos.isNonZero) {
          val stepDistance = cellRelPos.stepCount
          val value: Double = bot.view.cells(i) match {
            case CellType.ENEMY_MASTER => if (stepDistance <= 3) -150 else 0
            case CellType.ENEMY_SLAVE => 500 / stepDistance
            case CellType.MY_MASTER => -50 / stepDistance
            case CellType.MY_SLAVE => if (stepDistance < 3) -5 else -50 / stepDistance
            case CellType.WALL => if (stepDistance < 2) -10000 else -20 / stepDistance
            case _ => 1 / stepDistance
          }
          val direction45 = cellRelPos.toDirection45
          directionValue(direction45) += value
        }
        i += 1
      }
    }
    SharedControl.convertDirectionValueIntoMove(bot, directionValue)
  }
}
