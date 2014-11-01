package control

import analyzers.ExplosionAnalyzer
import utils.{MiniBot, XY}

/**
 * Shared weapon control functions
 */
object SharedWeaponControl {

  val ExplosionThreshold = 1.40
  val RequiredVisibleEnemies = 1

  /**
   * Finds most optimal blast radius and self-destructs.
   */
  def selfDestruct(bot: MiniBot) {
    val radiusAndDamage = ExplosionAnalyzer.apply(bot, bot.energy)
    bot.say("DIE[" + radiusAndDamage._1.toString + " - " + radiusAndDamage._2.toString  + "]")
    bot.explode(radiusAndDamage._1)

  }

  /**
   * Analyzes if it is valuable to explode now.
   * If valuable, executes explosion and returns true, else false.
   */
  def tryValuableExplosion(bot: MiniBot) = {
    if (bot.view.countVisibleEnemies() >= RequiredVisibleEnemies) {
      val radiusAndDamage = ExplosionAnalyzer.apply(bot, bot.energy)
      if (radiusAndDamage._2 > (bot.energy * ExplosionThreshold)) {
        bot.say("BOOM[" + radiusAndDamage._1.toString + " - " + radiusAndDamage._2.toString  + "]")
        bot.explode(radiusAndDamage._1)
        true
      }
    }
    false
  }

  /**
   * Analyzes if it is valuable launch a drop bomb.
   * If valuable, executes explosion and returns true, else false.
   */
  def tryDropBomb(bot: MiniBot) = {
    if (bot.view.countVisibleEnemies() >= RequiredVisibleEnemies && bot.energy > 300) {
      val radiusAndDamage = ExplosionAnalyzer.apply(bot, 100)
      if (radiusAndDamage._2 > (100 * ExplosionThreshold)) {
        val relPos = bot.view.offsetToNearestEnemy()
        bot.say("BOMB!")
        bot.spawn(relPos.signum, "type" -> "DropBomb")
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
