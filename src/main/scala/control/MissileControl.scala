package control

import analyzers.ExplosionAnalyzer
import utils.{MiniBot, View, XY}

/**
 * Main control for master bot.
 */
object MissileControl {

  var ExplosionThreshold = 1.10
  var RequiredVisibleEnemies = 2

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

    for (i <- 0 until view.cells.length) {
      val cellRelPos = view.relPosFromIndex(i)
      if (cellRelPos.isNonZero) {
        val stepDistance = cellRelPos.stepCount
        val value: Double = view.cells(i) match {
          case 'm' => if (stepDistance <= 3) -150 else (150 - stepDistance * 10) // enemy master
          case 's' => if (stepDistance <= 4) -150 else (120 - stepDistance * 10) // enemy slave
          case 'M' => -50 // my master
          case 'S' => -50 // friendly slave
          case 'B' => 15.0 // good beast
          case 'P' => 10.0 // good plant
          case 'b' => if (stepDistance <= 3) -150 else 0 // bad beast
          case 'p' => if (stepDistance < 2) -100 else 0 // bad plant
          case 'W' => if (stepDistance < 3) -200 else 0 // wall

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
    bot.say("BOOM!")
  }

  /**
   * Analyzes if it is valuable to explode now.
   * If valuable, executes explosion and returns true, else false.
   */
  def tryValuableExplosion(bot: MiniBot) = {
    if (bot.view.countVisibleEnemies() >= RequiredVisibleEnemies) {
      val radiusAndDamage = ExplosionAnalyzer.apply(bot)
      bot.status(radiusAndDamage._2.toString())
      if (radiusAndDamage._2 > (bot.energy * ExplosionThreshold)) {

        bot.explode(radiusAndDamage._1)
        true
      }
    }
    false
  }
}
