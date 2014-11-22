package control

import utils.Bot

/**
 * Main control for drop bomb.
 * Instant explosion.
 */
object DropBombControl {

  /**
   * Apply
   */
  def apply(bot: Bot) {
    val radius = bot.inputAsIntOrElse("radius", 5)
    bot.explode(radius)
  }
}
