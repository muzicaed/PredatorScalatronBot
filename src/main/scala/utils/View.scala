package utils

/**
 * Util for managing a bot's field of vision.
 */
case class View(cellsString: String) {
  //println("---- DUMP ------")
  //println(cellsString)
  val cells = cellsString.toCharArray
  val size = math.sqrt(cells.length).toInt
  val center = XY(size / 2, size / 2)

  def apply(relPos: XY) = cellAtRelPos(relPos)

  def cellAtRelPos(relPos: XY) = cells(indexFromRelPos(relPos))

  def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))

  def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size

  def absPosFromRelPos(relPos: XY) = relPos + center

  def cellAtAbsPos(absPos: XY) = cells(indexFromAbsPos(absPos))

  def offsetToNearestEnemy() = {
    val master = offsetToNearest('m').getOrElse(XY(1000, 1000))
    val slave = offsetToNearest('s').getOrElse(XY(1000, 1000))
    val beast = offsetToNearest('b').getOrElse(XY(1000, 1000))
    var nearest = master

    if (beast.length < master.length && beast.length < slave.length) {
      nearest = beast
    } else if (slave.length < master.length && slave.length < beast.length) {
      nearest = slave
    }

    nearest
  }

  def offsetToNearest(c: Char): Option[XY] = {
    var closestLength = 5000.0
    var closest:Option[XY] = None
    var i = 0
    while(i < cells.length) {
      if (cells(i) == c) {
        val cellRelPos = relPosFromIndex(i)
        val length = cellRelPos.length
        if (length < closestLength) {
          closestLength = length
          closest = Some(cellRelPos)
        }
      }
      i += 1
    }
    closest
  }

  def relPosFromIndexFromOffset(index: Int, relOffset: XY) = absPosFromIndex(index) - absPosFromRelPos(relOffset)

  def getRelPosForType(c: Char): Array[(Char, XY)] = {
    var i = 0
    var matches = new Array[(Char, XY)](0)
    while (i < cells.length) {
      if (cells(i) == c) {
        matches :+= (c, relPosFromIndex(i))
      }
      i += 1
    }
    matches
  }

  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))

  def absPosFromIndex(index: Int) = XY(index % size, index / size)

  def relPosFromAbsPos(absPos: XY) = absPos - center

  def countVisibleEnemies(): Int = {
    countType(CellType.ENEMY_MASTER) + countType(CellType.ENEMY_SLAVE) + countType(CellType.ENEMY_BEAST)
  }

  def countType(entityType: Char): Int = {
    var count = 0
    var i = 0
    while(i < cells.length) {
      if (cells(i) == entityType) count += 1
      i += 1
    }
    count
  }
}
