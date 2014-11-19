package control

import utils._

/**
 * Main control for master bot.
 */
object MasterControl {

  def apply(bot: MiniBot) {
    bot.status("-:[ pr3d470r - " + bot.slaves + "]:-")
    val moveDirection = analyzeView(bot)
    bot.move(moveDirection)

    // Check only if there is energy
    if (bot.energy > 100) {
      if (bot.time < 100 && bot.view.getRelPosForType(CellType.ENEMY_MASTER).length < 15) {
        val r = scala.util.Random
        SharedWeaponControl.spawnVampire(bot, XY.fromDirection45(r.nextInt(7)))
      }
      else if (!SharedWeaponControl.handleDanger(bot)) {
        if (checkEntitySpawn(bot)) {
          SharedWeaponControl.spawnVampire(bot, moveDirection.negate)
        } else if (SharedWeaponControl.checkFireMissile(bot)) {
          SharedWeaponControl.fireMissile(bot)
        }
      }
    }
  }

  /**
   * Check if now is a good time to spawn Vampire
   */
  def checkEntitySpawn(bot: Bot): Boolean = {
    bot.energy > 1000 && bot.slaves < Const.LOWER_SPAWN_LIMIT
  }

  /**
   * Analyze the view, building a map of attractiveness for the 45-degree directions and
   * recording other relevant data, such as the nearest elements of various kinds.
   */
  def analyzeView(bot: Bot) = {
    val directionValue = Array.ofDim[Double](8)

    var i = 0
    while (i < bot.view.cells.length) {
      val cellRelPos = bot.view.relPosFromIndex(i)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = bot.view.cells(i) match {
          case CellType.ENEMY_MASTER =>
            if (stepDistance > 10) -40 / stepDistance else -50

          case CellType.ENEMY_SLAVE =>
            if (bot.energy < 10000) -150 / stepDistance
            else -30 / stepDistance

          case CellType.FOOD_BEAST =>
            if (bot.energy < 10000) 200 / stepDistance else 0

          case CellType.ENEMY_BEAST =>
            if (stepDistance < 3) -500
            else -130 / stepDistance

          case CellType.MY_SLAVE => 40 / stepDistance
          case CellType.FOOD_PLANT => if (bot.energy < 10000) 150 / stepDistance else 0
          case CellType.ENEMY_PLANT => if (stepDistance < 3) -80 else 0
          case CellType.WALL => if (stepDistance < 2) -10000 else -20 / stepDistance
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
