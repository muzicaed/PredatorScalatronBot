package control

import utils.{Bot, MiniBot, View, XY}

/**
 * Main Controller
 */
object ControlFunction
{
  def forHunter(bot: Bot) {
    val (directionValue, enemies) = analyzeViewAsHunter(bot)
    val dontFireMissileUntil = bot.inputAsIntOrElse("dontFireMissileUntil", -1)
    val cloneCount = bot.inputAsIntOrElse("cloneCount", 0)

    var energyTransfer = 0.05
    if (bot.energy > 2000) energyTransfer = 0.10
    else if (bot.energy > 5000) energyTransfer = 0.15
    else if (bot.energy > 15000) energyTransfer = 0.35

    val moveDirection = moveBotInDirection(bot, directionValue)

    bot.set("cloneCount" -> (cloneCount + 1))
    if ((bot.energy > 5000 && cloneCount > 20) || (bot.energy > 2000 && cloneCount > 70)) {
      bot.spawn(moveDirection.negate, "mood" -> "Clone", "energy" -> (bot.energy * energyTransfer).toInt)
      bot.set("cloneCount" -> 0)
      bot.say("Rise from the dead!")
    }

    if(enemies.length > 0 && dontFireMissileUntil < bot.time) {
      var fireRate = 1.5
      var power = 100

      if (bot.energy > 2000) {
        fireRate = 0.8
        power = 110
      } else if (bot.energy > 4000) {
        fireRate = 0.6
        power = 120
      } else if (bot.energy > 10000) {
        fireRate = 0.4
        power = 130
      }
      bot.set("dontFireMissileUntil" -> (bot.time + (enemies.length * fireRate).toInt))
      if (bot.energy > 1000) {
        for (enemyPos <- enemies) {
          val unitDelta = enemyPos.signum
          bot.spawn(unitDelta, "mood" -> "Missile", "target" -> unitDelta, "energy" -> power)
        }
      }
    }
  }


  def forSlave(bot: MiniBot) {
    bot.inputOrElse("mood", "Unknowned") match {
      case "Missile" => reactAsMissile(bot)
      case "Clone" =>
        bot.status("Vampire")
        forHunter(bot)
      case s: String => bot.log("unknown mood: " + s)
    }
  }


  def reactAsMissile(bot: MiniBot) {
    bot.view.offsetToNearest('m') match {
      case Some(delta: XY) =>
        // close enough to blow it up?
        if(delta.length <= 2) {
          bot.explode(3)
          return
        }
      case None =>
    }

    bot.view.offsetToNearest('s') match {
      case Some(delta: XY) =>
        // close enough to blow it up?
        if(delta.length <= 3) {
          bot.explode(6)
          return
        }
      case None =>
    }

    val target = bot.inputAsXYOrElse("target", XY.Zero)
    val directionValue = analyzeViewAsMissile(bot.view)
    directionValue(target.toDirection45) += 500
    moveBotInDirection(bot, directionValue)
  }


  def moveBotInDirection(bot: Bot, directionValue: Array[Double]) = {
    val lastDirection = bot.inputAsIntOrElse("lastDirection", 0)

    // determine movement direction
    directionValue(lastDirection) += 100 // try to break ties by favoring the last direction
    val bestDirection45 = directionValue.zipWithIndex.maxBy(_._1)._2
    val direction = XY.fromDirection45(bestDirection45)
    bot.move(direction)
    bot.set("lastDirection" -> bestDirection45)
    direction
  }

  /** Analyze the view, building a map of attractiveness for the 45-degree directions and
    * recording other relevant data, such as the nearest elements of various kinds.
    */
  def analyzeViewAsHunter(bot: Bot) = {
    val view = bot.view
    val directionValue = Array.ofDim[Double](8)
    var enemies = Array[XY]()

    val cells = view.cells
    val cellCount = cells.length
    for(i <- 0 until cellCount) {
      val cellRelPos = view.relPosFromIndex(i)
      if(cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = cells(i) match {
          case 'm' => // another master: not dangerous, but an obstacle
            enemies +:= cellRelPos
            if(stepDistance < 2 || bot.energy < 5000) -1000
            else (1000 - stepDistance * 100)

          case 'M' => // another master: not dangerous, but an obstacle
            -500

          case 's' => // another slave: potentially dangerous?
            enemies +:= cellRelPos
            if (bot.energy < 2000) -1000
            else (1500 - stepDistance * 150)

          case 'S' => // out own slave
            -500

          case 'B' => // good beast: valuable, but runs away
            if(stepDistance == 1) 600
            else if(stepDistance == 2) 300
            else (150 - stepDistance * 15)

          case 'P' => // good plant: less valuable, but does not run
            if(stepDistance < 4) 500 else 0

          case 'b' => // bad beast: dangerous, but only if very close
            if(stepDistance < 4) -400 / stepDistance else -50 / stepDistance

          case 'p' => // bad plant: bad, but only if I step on it
            if(stepDistance < 2) -1000 else 0

          case 'W' => // wall: harmless, just don't walk into it
            if(stepDistance < 2) -1500 else 0

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    (directionValue, enemies)
  }

  def analyzeViewAsMissile(view: View) = {
    val directionValue = Array.ofDim[Double](8)

    val cells = view.cells
    val cellCount = cells.length
    for(i <- 0 until cellCount) {
      val cellRelPos = view.relPosFromIndex(i)
      if(cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = cells(i) match {
          case 'm' => // another master: not dangerous, but an obstacle
            (1500 - stepDistance * 10)

          case 's' => // another slave: potentially dangerous?
            (2000 - stepDistance * 10)

          case 'B' => // good beast: valuable, but runs away
            0.0

          case 'P' => // good plant: less valuable, but does not run
            0.0

          case 'b' => // bad beast: dangerous, but only if very close
            if(stepDistance < 3) -1500 else 0

          case 'p' => // bad plant: bad, but only if I step on it
            if(stepDistance < 2) -1000 else 0

          case 'W' => // wall: harmless, just don't walk into it
            if(stepDistance < 4) -2000 else 0

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    directionValue
  }
}