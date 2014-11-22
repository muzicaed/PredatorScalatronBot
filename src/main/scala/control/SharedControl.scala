package control

import utils._

/**
 * Shared control functions
 */
object SharedControl {

  /**
   * Spawns a clone bot and transfers all energy = warp (move two steps).
   */
  def warpBotInDirection(bot: Bot, moveDirection: XY, warpDirection: XY): Unit = {
    if (bot.energy > 100) {
      val botType = bot.inputOrElse("type", "invalid")
      val energy = handleEnergyBeforeWarp(bot, moveDirection)

      bot.spawn(warpDirection, "type" -> botType, "target" -> warpDirection.toDirection45, "energy" -> energy)
    }
  }

  /**
   * Handles energy changes during this move.
   * To calculate the real energy to transfer to warp clone.
   */
  def handleEnergyBeforeWarp(bot: Bot, moveDirection: XY): Int = {
    var energy = bot.energy
    if ((bot.time % Const.SLAVE_DEPLETION_CYCLE_STEPS) == 0 && bot.energy > Const.SLAVE_DEPLETION_PER_CYCLE) {
      energy -= Const.SLAVE_DEPLETION_PER_CYCLE
    }

    val moveCell = bot.view.cellAtRelPos(moveDirection)
    val energyMod = moveCell match {
      case CellType.FOOD_BEAST => 200
      case CellType.FOOD_PLANT => 100
      case CellType.ENEMY_BEAST => -150
      case CellType.ENEMY_PLANT => -100
      case _ => 0
    }
    energy + energyMod
  }

  /**
   * Converts result from view analysis into XY move.
   * Will apply last direction if applicable.
   */
  def convertDirectionValueIntoMove(bot: Bot, directionValue: Array[Double]): XY = {
    val direction = bot.inputAsIntOrElse("target", 0)
    directionValue(direction) += 30 // try to break ties by favoring the last direction

    var bestDirection45 = 0
    var bestScore = 0.0
    var i = 0
    while(i < directionValue.length) {
      val score = directionValue(i)
      if (score > bestScore) {
        bestScore = score
        bestDirection45 = i
      }
      i += 1
    }

    bot.set("target" -> bestDirection45)
    XY.fromDirection45(bestDirection45).signum
  }
}