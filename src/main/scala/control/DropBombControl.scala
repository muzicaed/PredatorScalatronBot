package control

import utils.MiniBot

/**
 * Main control for drop bomb.
 * Instant explosion.
 */
object DropBombControl {

  /**
   * Apply
   */
  def apply(bot: MiniBot) {
    val radius = bot.inputAsIntOrElse("radius", 5)
    bot.explode(radius)
  }
}
