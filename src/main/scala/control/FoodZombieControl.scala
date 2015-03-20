package control

import utils._

/**
 * Main control for food zombie bot.
 * The Food zombie will just look for food and return to master
 * when it has harvested enough.
 * Vampires will turn into food zombies when now enemies is around,
 * and food zombies turn back into a vampire if it spots an enemy.
 */
object FoodZombieControl {

  def apply(bot: Bot) {
    if (Const.DEBUG && bot.energy > 0) bot.status("Zombie [" + bot.energy.toString + "]")

    var headHome = false
    if ((bot.energy > 2000 && bot.offsetToMaster.length <= 20) || bot.energy > 4000 || bot.apocalypse < 100) {
      headHome = true
    }
    val moveDirection = move(bot, headHome)

    if (bot.energy > 100 && !SharedWeaponControl.tryDropBomb(bot)) {
      if (!SharedWeaponControl.tryValuableExplosion(bot))
      {
        if (bot.time < 20 || headHome) {
          val warpDirection = analyzeView(bot, moveDirection, headHome)
          SharedControl.warpBotInDirection(bot, moveDirection, warpDirection)
        }
      }
    }

    if (!checkSafe(bot)) bot.set("type" -> SlaveType.VAMPIRE)
  }

  /**
   * Checks if there are no visible enemy bots.
   */
  def checkSafe(bot: Bot): Boolean = {
    val enemyBotCount = bot.view.countType(CellType.ENEMY_MASTER) + bot.view.countType(CellType.ENEMY_SLAVE)
    enemyBotCount == 0
  }

  /**
   * Moves this bot.
   */
  def move(bot: Bot, headHome: Boolean): XY = {
    var moveDirection = analyzeView(bot, XY.Zero, headHome)
    var foundMove = false
    var count = 0

    while (!foundMove) {
      val cell = bot.view.cellAtRelPos(moveDirection)
      if (CellType.canMoveTo(cell)) {
        bot.move(moveDirection)
        foundMove = true
      } else {
        moveDirection = moveDirection.negate.rotateClockwise45
      }

      if (count > 7) {
        foundMove = true
      }
      count += 1
    }

    moveDirection
  }

  /**
   * Analyze the view and most valuable choose direction.
   */
  def analyzeView(bot: Bot, offsetPos: XY, headHome: Boolean) = {
    val directionValue = Array.ofDim[Double](8)
    var i = 0
    while (i < bot.view.cells.length) {
      val cellRelPos = bot.view.relPosFromIndexFromOffset(i, offsetPos)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = bot.view.cells(i) match {
          case CellType.ENEMY_MASTER => -200 / stepDistance
          case CellType.ENEMY_SLAVE => -200 / stepDistance

          case CellType.FOOD_BEAST =>
            if (stepDistance == 1) 100
            else if (stepDistance < 4) 100
            else 100 / stepDistance

          case CellType.ENEMY_BEAST =>
            if (bot.energy < 200) {
              -100 / stepDistance
            } else {
              if (stepDistance < 2) -1000 / stepDistance else 110 / stepDistance
            }

          case CellType.MY_SLAVE => -200 / stepDistance
          case CellType.MY_MASTER => if (headHome) 200 / stepDistance else -30 / stepDistance
          case CellType.FOOD_PLANT => if (stepDistance < 3) 80 else 0
          case CellType.ENEMY_PLANT => if (stepDistance < 3) -80 else 0
          case CellType.WALL => if (stepDistance < 2) -10000 else -10 / stepDistance
          case _ => 5 / stepDistance
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
