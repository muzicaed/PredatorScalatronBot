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
    val center = XY.Zero
    val master = offsetToNearest('m') getOrElse XY(1000, 1000)
    val slave = offsetToNearest('s') getOrElse XY(1000, 1000)
    val beast = offsetToNearest('b') getOrElse XY(1000, 1000)
    var nearest = master

    if (center.distanceTo(beast) < center.distanceTo(master) && center.distanceTo(beast) < center.distanceTo(slave)) {
      nearest = beast
    } else if (center.distanceTo(slave) < center.distanceTo(master) && center.distanceTo(slave) < center.distanceTo(beast)) {
      nearest = slave
    }

    nearest
  }

  def offsetToNearest(c: Char): Option[XY] = {
    var closestDistance = 500.0
    var closest:Option[XY] = None
    var i = 0
    while(i < cells.length) {
      if (cells(i) == c) {
        val cellRelPos = relPosFromIndex(i)
        val distance = XY.Zero.distanceTo(cellRelPos)
        if (distance < closestDistance) {
          closestDistance = distance
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
    val matches = new Array[(Char, XY)](cells.length)
    while (i < cells.length) {
      if (cells(i) == c) {
        matches :+(c, relPosFromIndex(i))
      }
      i += 1
    }
    matches
  }

  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))

  def absPosFromIndex(index: Int) = XY(index % size, index / size)

  def relPosFromAbsPos(absPos: XY) = absPos - center

  def countVisibleEnemies(): Int = {
    countType('m') + countType('s') + countType('b')
  }

  def countType(c: Char): Int = {
    cellsString.count(_ == c)
  }
}
