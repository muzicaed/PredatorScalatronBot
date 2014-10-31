package utils

/**
 * Represents a MiniBot (Spawn)
 */
trait MiniBot extends Bot {
  // inputs
  def offsetToMaster: XY

  // outputs
  def explode(blastRadius: Int) : Bot
}
