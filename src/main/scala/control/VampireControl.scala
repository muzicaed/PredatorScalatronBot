package control

import utils._

/**
 * Main control for vampire bot.
 * The Vampire is a slightly more aggressive "mini master".
 * Collects food like master, but will use more fire power.
 * Will spawn more vampires.
 */
object VampireControl {

  def apply(bot: MiniBot) {
    if (Const.DEBUG && bot.energy > 0) bot.status("Vamp [" + bot.energy.toString + "]")

    if (bot.time < 100) {
      val moveDirection = move(bot, false)
      SharedWeaponControl.spawnVampire(bot, moveDirection.negate)
    } else {
      if (SharedWeaponControl.shouldSelfDestruct(bot)) {
        SharedWeaponControl.selfDestruct(bot)
      } else {
        var headHome = false
        if ((bot.energy > 5000 && bot.offsetToMaster.length <= 15) || bot.apocalypse < 150) {
          headHome = true
        }
        val moveDirection = move(bot, headHome)

        if (!SharedWeaponControl.tryDropBomb(bot)) {
          if (!SharedWeaponControl.tryValuableExplosion(bot)) {
            if (SharedWeaponControl.checkFireMissile(bot)) {
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

    if (bot.energy < 150) {
      //bot.set("type" -> SlaveType.HUNTER)
    }
  }

  /**
   * Moves this bot.
   */
  def move(bot: MiniBot, headHome: Boolean): XY = {
    var moveDirection = analyzeView(bot, XY.Zero, headHome)
    var foundMove = false
    var count = 0

    while(!foundMove) {
      val cell = bot.view.cellAtRelPos(moveDirection)
      if (CellType.canMoveTo(cell)) {
        bot.move(moveDirection)
        foundMove = true
      } else {
        moveDirection = moveDirection.rotateClockwise90.rotateClockwise45
      }

      if (count > 7) {
        foundMove = true
      }
      count += 1
    }

    moveDirection
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
          case CellType.ENEMY_MASTER =>
            if (stepDistance < 7 || bot.energy < 400 || bot.time < 200) -200
            else 200 / stepDistance

          case CellType.ENEMY_SLAVE => // enemy slave
            if (stepDistance < 7 || bot.energy < 400 || bot.time < 200) -250
            else 100 / stepDistance

          case CellType.FOOD_BEAST => // good beast
            if (stepDistance == 1) 100
            else if (stepDistance < 4) 80
            else 80 / stepDistance

          case CellType.ENEMY_BEAST =>
            if (bot.energy < 200) {
              -100 / stepDistance
            } else {
              if (stepDistance < 2) -1000 / stepDistance else 110 / stepDistance
            }

          case CellType.MY_SLAVE =>  -200 / stepDistance
          case CellType.MY_MASTER => if (headHome) 200 / stepDistance else -30 / stepDistance
          case CellType.FOOD_PLANT => if (stepDistance < 3) 80 else 0
          case CellType.ENEMY_PLANT => if (stepDistance < 3) -80 else 0
          case CellType.WALL => if (stepDistance < 2) -10000 else -20 / stepDistance
          case _ => 1 / stepDistance
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
