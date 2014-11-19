package utils

/**
 * Enum for Entity types
 */
object CellType {
  type Type = Char
  val MY_MASTER = 'M'
  val ENEMY_MASTER = 'm'
  val MY_SLAVE = 'S'
  val ENEMY_SLAVE = 's'
  val FOOD_PLANT = 'P'
  val ENEMY_PLANT = 'p'
  val FOOD_BEAST = 'B'
  val ENEMY_BEAST = 'b'
  val EMPTY = '_'
  val WALL = 'W'
  val UNKNOWN = '?'

  /**
   * Checks if mini bot can move to cell.
   */
  def canMoveTo(cell: Char): Boolean = {
    cell != WALL && cell != ENEMY_BEAST && cell != ENEMY_PLANT && cell != MY_SLAVE && cell != ENEMY_SLAVE && cell != ENEMY_MASTER
  }

  /**
   * Checks if master bot can move to cell.
   */
  def canMasterMoveTo(cell: Char): Boolean = {
    cell != WALL && cell != ENEMY_BEAST && cell != ENEMY_PLANT && cell != ENEMY_MASTER
  }
}
