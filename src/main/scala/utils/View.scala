package utils

/**
 * Util for managing a bot's field of vision.
 */
case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size / 2, size / 2)

  def apply(relPos: XY) = cellAtRelPos(relPos)

  def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

  def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))

  def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size

  def absPosFromRelPos(relPos: XY) = relPos + center

  def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

  def offsetToNearest(c: Char) = {
    val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
    if (matchingXY.isEmpty)
      None
    else {
      val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
      Some(nearest)
    }
  }

  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))

  def absPosFromIndex(index: Int) = XY(index % size, index / size)

  def relPosFromAbsPos(absPos: XY) = absPos - center

  def getRelPosForType(c: Char): List[(Char, XY)] = {
    var matches = List[(Char, XY)]()
    val matchingCells = cells.view.zipWithIndex.filter(_._1 == c)

    if (!matchingCells.isEmpty) {
      matchingCells.foreach {
        case (typeChar, index) => {
          if (typeChar == c) {
            matches = (c, relPosFromIndex(index)) :: matches
          }
        }
      }
    }
    matches
  }

  def countVisibleEnemies(): Int = {
    countType('m') + countType('s') + countType('b')
  }

  def countType(c: Char): Int = {
    cells.count(_ == c)
  }
}
