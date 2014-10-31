package control

import analyzers.ExplosionAnalyzer
import utils.{MiniBot, View, XY}

/**
 * Main control for master bot.
 */
object MissileControl {

  var ExplosionThreshold = 1.3
  var RequiredVisibleEnemies = 5

  /**
   * Apply
   */
  def apply(bot: MiniBot) {
    if (shouldSelfDestruct(bot)) {
      bot.say("SELF DESTRUCT!")
      selfDestruct(bot)
    } else if (!tryValuableExplosion(bot)) {
      move(bot)
    }
  }

  /**
   * Checks if enemies are too close and
   * that self destruct would be a good option.
   */
  def shouldSelfDestruct(bot: MiniBot): Boolean = {
    bot.view.offsetToNearest('m') match {
      case Some(delta: XY) =>
        // Too close, self destruct!
        if (delta.length <= 1) {
          return true
        }
      case None =>
    }

    bot.view.offsetToNearest('s') match {
      case Some(delta: XY) =>
        // Too close, self destruct!
        if (delta.length <= 1) {
          return true
        }
      case None =>
    }

    false
  }

  /**
   * Moves the bot.
   */
  def move(bot: MiniBot) {
    val target = bot.inputAsXYOrElse("target", XY.Zero)
    val directionValue = analyzeView(bot.view)
    directionValue(target.toDirection45) += 10
    SharedControl.moveBotInDirection(bot, directionValue)
  }

  /**
   * Analyze the view.
   */
  def analyzeView(view: View) = {
    val directionValue = Array.ofDim[Double](8)

    val cells = view.cells
    val cellCount = cells.length
    for (i <- 0 until cellCount) {
      val cellRelPos = view.relPosFromIndex(i)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = cells(i) match {
          case 'm' => // another master: not dangerous, but an obstacle
            if (stepDistance <= 3) -150 else (150 - stepDistance * 10)


          case 's' => // another slave: potentially dangerous?
            if (stepDistance <= 4) -150 else (120 - stepDistance * 10)


          case 'B' => // good beast: valuable, but runs away
            0.0

          case 'P' => // good plant: less valuable, but does not run
            0.0

          case 'b' => // bad beast: dangerous, but only if very close
            if (stepDistance <= 3) -150 else 0

          case 'p' => // bad plant: bad, but only if I step on it
            if (stepDistance < 2) -100 else 0

          case 'W' => // wall: harmless, just don't walk into it
            if (stepDistance < 3) -200 else 0

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    directionValue
  }

  /**
   * Finds most optimal blast radius and self-destructs.
   */
  def selfDestruct(bot: MiniBot) {
    val radiusAndDamage = ExplosionAnalyzer.apply(bot)
    bot.explode(radiusAndDamage._1)
  }

  /**
   * Analyzes if it is valuable to explode now.
   * If valuable, executes explosion and returns true, else false.
   */
  def tryValuableExplosion(bot: MiniBot) = {
    if (bot.view.countVisibleEnemies() >= RequiredVisibleEnemies) {
      val radiusAndDamage = ExplosionAnalyzer.apply(bot)
      bot.status(radiusAndDamage._2.toString())
      println("Damage: " + radiusAndDamage._2)
      println("Threshold: " + (bot.energy * ExplosionThreshold))
      if (radiusAndDamage._2 > (bot.energy * ExplosionThreshold)) {

        bot.explode(radiusAndDamage._1)
        println("BOOM")
        true
      }
      println("----")
    }
    false
  }
}
