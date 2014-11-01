package control

import analyzers.ExplosionAnalyzer
import utils.{MiniBot, XY}

/**
 * Shared weapon control functions
 */
object SharedWeaponControl {

  val ExplosionThreshold = 0.9
  val RequiredVisibleEnemies = 2

  /**
   * Finds most optimal blast radius and self-destructs.
   */
  def selfDestruct(bot: MiniBot) {
    val radiusAndDamage = ExplosionAnalyzer.apply(bot)
    bot.say("DIE[" + radiusAndDamage._1.toString + " - " + radiusAndDamage._2.toString  + "]")
    bot.explode(radiusAndDamage._1)

  }

  /**
   * Analyzes if it is valuable to explode now.
   * If valuable, executes explosion and returns true, else false.
   */
  def tryValuableExplosion(bot: MiniBot) = {
    if (bot.view.countVisibleEnemies() >= RequiredVisibleEnemies) {
      val radiusAndDamage = ExplosionAnalyzer.apply(bot)
      if (radiusAndDamage._2 > (bot.energy * ExplosionThreshold)) {
        bot.say("BOOM[" + radiusAndDamage._1.toString + " - " + radiusAndDamage._2.toString  + "]")
        bot.explode(radiusAndDamage._1)
        true
      }
    }
    false
  }

  /**
   * Checks if enemies are too close and
   * that self destruct would be a good option.
   */
  def shouldSelfDestruct(bot: MiniBot): Boolean = {
    bot.view.offsetToNearest('s') match {
      case Some(delta: XY) =>
        // Too close, self destruct!
        if (delta.length <= 2) {
          return true
        }
      case None =>
    }

    bot.view.offsetToNearest('s') match {
      case Some(delta: XY) =>
        // Too close, self destruct!
        if (delta.length <= 2) {
          return true
        }
      case None =>
    }

    false
  }
}
