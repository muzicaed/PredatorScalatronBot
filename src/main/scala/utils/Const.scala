package utils

/**
 * Enum for types of slave bots.
 */
object Const {
  val DEBUG = false
  val MIN_BLAST_RADIUS = 2
  val MAX_BLAST_RADIUS = 10
  val EXPLOSION_DAMAGE_FACTOR = 200
  val MAX_DAMAGE_BEAST = 200
  val MAX_DAMAGE_SLAVE = 110
  val MAX_DAMAGE_MASTER = 500
  val VALUABLE_EXPLOSION_THRESHOLD = 1.75

  val SLAVE_DEPLETION_CYCLE_STEPS = 4
  val SLAVE_DEPLETION_PER_CYCLE = 1
  val LOWER_SPAWN_LIMIT = 90
  val UPPER_SPAWN_LIMIT = 150

  val USELESS_THRESHOLD = 25
}
