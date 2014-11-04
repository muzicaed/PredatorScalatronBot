package control

import utils.{Bot, MiniBot, XY}

/**
 * Main control for hunter bot.
 * The hunter avoids conflict with other bots and tries
 * to hunt food.
 * After growing strong it evolves into a vampire.
 */
object HunterControl {

  def apply(bot: MiniBot) {
    if (bot.energy > 0) {
      bot.status("Hunter [" + bot.energy.toString + "]")
      if (SharedWeaponControl.shouldSelfDestruct(bot)) {
        SharedWeaponControl.selfDestruct(bot)
      } else {
        if (!SharedWeaponControl.handleDanger(bot)) {
          if (!SharedWeaponControl.tryDropBomb(bot)) {
            val moveDirection = analyzeView(bot, XY.Zero)
            bot.move(moveDirection)

            if (bot.energy > 1000) {
              bot.say("Bloood!")
              bot.set("type" -> "Vampire")
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

      for (i <- 0 until bot.view.cells.length) {
        val cellRelPos = bot.view.relPosFromIndexFromOffset(i, offsetPos)
        if (cellRelPos.isNonZero) {
          val stepDistance = cellRelPos.stepCount
          val value: Double = bot.view.cells(i) match {
            case 'm' => -200 // another master
            case 's' => -500 // enemy slave

            case 'B' => // good beast
              if (stepDistance <= 4) 500
              else 190 / stepDistance

            case 'b' => // bad beast
              if (stepDistance < 2) -500
              else -100 / stepDistance

            case 'S' => if (stepDistance < 2) 0 else -100 // friendly slave
            case 'M' => -100 // friendly master
            case 'P' => 200 / stepDistance // good plant
            case 'p' => if (stepDistance < 3) -80 else 0 // bad plant
            case 'W' => if (stepDistance < 3) -10000 else 0 // wall
            case _ => 0.0
          }
          val direction45 = cellRelPos.toDirection45
          directionValue(direction45) += value
        }
      }
      val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
      XY.fromDirection45(bestDirection45)
    }
  }
}
