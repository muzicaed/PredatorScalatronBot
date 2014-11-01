package control

import analyzers.ExplosionAnalyzer
import utils.{MiniBot, View}

/**
 * Main control for drop bomb.
 * Instant explosion.
 */
object DropBombControl {

  /**
   * Apply
   */
  def apply(bot: MiniBot) {
    val radiusAndDamage = ExplosionAnalyzer.apply(bot, bot.energy)
    bot.explode(radiusAndDamage._1)
  }
}
