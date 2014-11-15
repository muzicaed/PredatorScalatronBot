package control

import utils._

/**
 * Shared control functions
 */
object SharedControl {

  /**
   * Spawns a clone bot and transfers all energy = warp (move two steps).
   */
  def warpBotInDirection(bot: MiniBot, moveDirection: XY, warpDirection: XY): Unit = {
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
  def handleEnergyBeforeWarp(bot: MiniBot, moveDirection: XY): Int = {
    var energy = bot.energy
    if ((bot.time % Const.SlaveDepletionCycleSteps) == 0 && bot.energy > Const.SlaveDepletionPerCycle) {
      energy -= Const.SlaveDepletionPerCycle
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
    directionValue(direction) += 15 // try to break ties by favoring the last direction

    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    bot.set("target" -> bestDirection45)
    XY.fromDirection45(bestDirection45).signum
  }
}