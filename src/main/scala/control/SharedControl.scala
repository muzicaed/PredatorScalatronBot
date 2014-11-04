package control

import utils.{MiniBot, XY}

/**
 * Shared control functions
 */
object SharedControl {

  val SlaveDepletionCycleSteps = 4
  val SlaveDepletionPerCycle = 1

  /**
   * Moves bot in direction and stores as last direction.
   */
  def moveBotInDirection(bot: MiniBot, directionValue: Array[Double]) = {
    val lastDirection = bot.inputAsIntOrElse("lastDirection", 0)

    // If Mini-Bot and apocalypse closing in, head home!
    if (bot.generation > 0 && bot.apocalypse < 90) {
      val directionXY = bot.offsetToMaster
      directionValue(directionXY.toDirection45) += 10000
    }

    // determine movement direction
    directionValue(lastDirection) += 70 // try to break ties by favoring the last direction
    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    val direction = XY.fromDirection45(bestDirection45)

    bot.move(direction)
    bot.set("lastDirection" -> bestDirection45)
    direction
  }

  /**
   * Spawns a clone bot and transfers all energy = warp (move two steps).
   */
  def warpBotInDirection(bot: MiniBot, moveDirection: XY, warpDirection: XY): Unit = {
    if (bot.energy > 100) {
      val target = bot.inputAsXYOrElse("target", XY.Zero)
      val botType = bot.inputOrElse("type", "invalid")
      val energy = handleEnergyBeforeWarp(bot, moveDirection)

      bot.spawn(warpDirection.signum, "type" -> botType, "target" -> target, "energy" -> energy)
    }
  }

  /**
   * Handles energy changes during this move.
   * To calculate the real energy to transfer to warp clone.
   */
  def handleEnergyBeforeWarp(bot: MiniBot, moveDirection: XY): Int = {
    var energy = bot.energy
    if ((bot.time % SlaveDepletionCycleSteps) == 0 && bot.energy > SlaveDepletionPerCycle) {
      energy -= SlaveDepletionPerCycle
    }

    val moveCell = bot.view.cellAtRelPos(moveDirection)
    val energyMod = moveCell match {
      case 'B' => 200 // Good beast
      case 'P' => 100 // Good plant
      case 'b' => -150 // Bad beast
      case 'p' => -100 // Bad plant
      case _ => 0
    }
    energy + energyMod
  }
}