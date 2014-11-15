package utils

/**
 * Enum for Scalatron Entity types
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
}
