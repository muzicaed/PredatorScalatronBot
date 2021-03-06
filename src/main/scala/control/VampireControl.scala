package control

import utils._

/**
 * Main control for vampire bot.
 * The Vampire is a slightly more aggressive "mini master".
 * Collects food like master, but will use more fire power.
 * Will spawn more vampires.
 * Will turn into food zombie if no enemies are around for some time.
 */
object VampireControl {

  def apply(bot: Bot) {
    if (Const.DEBUG && bot.energy > 0) bot.status("Vamp [" + bot.energy.toString + "]")

    if (bot.time < 70) {
      val moveDirection = move(bot, headHome = false)
      SharedWeaponControl.spawnVampire(bot, moveDirection.negate)
    } else {
      val isSafe = checkSafe(bot)

      if (SharedWeaponControl.shouldSelfDestruct(bot)) {
        SharedWeaponControl.selfDestruct(bot)
      } else {
        var headHome = false
        if ((bot.energy > 2000 && bot.offsetToMaster.length <= 20) || bot.apocalypse < 100) {
          headHome = true
        }
        val moveDirection = move(bot, headHome)

        if (bot.energy > 100 && !SharedWeaponControl.tryDropBomb(bot)) {
          if (!isSafe && !SharedWeaponControl.tryValuableExplosion(bot)) {
            if (!isSafe && SharedWeaponControl.checkFireMissile(bot)) {
              SharedWeaponControl.fireMissile(bot)
            }
            else if (!headHome && bot.energy > 300 && bot.slaves < Const.LOWER_SPAWN_LIMIT && bot.view.countType('S') < 2) {
              SharedWeaponControl.spawnVampire(bot, moveDirection.negate)
            } else if (bot.time < 20 || headHome) {
              val warpDirection = analyzeView(bot, moveDirection, headHome)
              SharedControl.warpBotInDirection(bot, moveDirection, warpDirection)
            }
          }
        }
      }
    }
  }

  /**
   * Checks if there are no visible enemy bots.
   */
  def checkSafe(bot: Bot): Boolean = {
    val enemyBotCount = bot.view.countType(CellType.ENEMY_MASTER) + bot.view.countType(CellType.ENEMY_SLAVE)
    val noEnemiesTurns = if (enemyBotCount == 0) bot.inputAsIntOrElse("noEnemiesTurns", 0) + 1 else 0
    if (noEnemiesTurns > 5) bot.set("type" -> SlaveType.FOOD_ZOMBIE)
    bot.set("noEnemiesTurns" -> noEnemiesTurns)

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
          case CellType.ENEMY_MASTER =>
            if (stepDistance < 7 || bot.energy < 400 || bot.time < 200) -200
            else 200 / stepDistance

          case CellType.ENEMY_SLAVE =>
            if (stepDistance < 7 || bot.energy < 400 || bot.time < 200) -250
            else 100 / stepDistance

          case CellType.FOOD_BEAST =>
            if (stepDistance == 1) 100
            else if (stepDistance < 4) 80
            else 80 / stepDistance

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
