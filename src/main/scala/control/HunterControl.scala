package control

import utils._

/**
 * Main control for hunter bot.
 * The hunter avoids conflict with other bots and tries
 * to hunt food.
 * After growing strong it evolves into a vampire.
 */
object HunterControl {

  def apply(bot: MiniBot) {
    if (Const.DEBUG && bot.energy > 0) bot.status("Hunter [" + bot.energy.toString + "]")
    if (SharedWeaponControl.shouldSelfDestruct(bot)) {
      SharedWeaponControl.selfDestruct(bot)
    } else {
      val moveDirection = analyzeView(bot, XY.Zero)
      bot.move(moveDirection)
      if (!SharedWeaponControl.handleDanger(bot)) {
        if (!SharedWeaponControl.tryDropBomb(bot)) {
          if (bot.energy > 1200) {
            bot.set("type" -> SlaveType.VAMPIRE)
            if (Const.DEBUG) bot.say("Bloood!")
          } else {
            val warpDirection = analyzeView(bot, moveDirection.signum)
            SharedControl.warpBotInDirection(bot, moveDirection, warpDirection)
          }
        }
      }
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
          case CellType.ENEMY_MASTER => -100 / stepDistance
          case CellType.ENEMY_SLAVE => -300 / stepDistance

          case CellType.FOOD_BEAST =>
            if (stepDistance <= 4) 500
            else 190 / stepDistance

          case CellType.ENEMY_BEAST =>
            if (stepDistance < 2) -500
            else -100 / stepDistance

          case CellType.MY_SLAVE => if (stepDistance < 2) -1000 else -200 / stepDistance
          case CellType.MY_MASTER => -100 / stepDistance
          case CellType.FOOD_PLANT => 200 / stepDistance
          case CellType.ENEMY_PLANT => if (stepDistance < 3) -80 else 0
          case CellType.WALL => if (stepDistance < 2) -10000 else -20 / stepDistance // wall
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
