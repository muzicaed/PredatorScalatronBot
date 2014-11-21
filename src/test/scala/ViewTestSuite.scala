import org.scalatest.FunSuite
import utils.{XY, Time, View}

/**
 * Tests for View
 */
class ViewTestSuite extends FunSuite {

  test("Should count visible enemies") {
    val view = new View(miniBotMap)
    val count = Time("countVisibleEnemies", {view.countVisibleEnemies()})
    assert(count == 2)
  }

  test("Should count visible of type") {
    val view = new View(miniBotMap)
    val count = Time("countType", {view.countType('W')})
    assert(count == 16)
  }

  test("Should give the distance to closest enemy") {
    val view = new View(miniBotMap)
    val offset = Time("offsetToNearestEnemy", {view.offsetToNearestEnemy()})
    assert(offset == new XY(6, -9))
  }

  test("Should give the distance to closest of type") {
    val view = new View(miniBotMap)
    val offsetW = Time("offsetToNearest W", {view.offsetToNearest('W')})
    assert(offsetW == Some(new XY(-4, -7)))

    val offsetM = Time("offsetToNearest M", {view.offsetToNearest('M')})
    assert(offsetM == None)
  }

  test("Should get the relative position for closest of type") {
    val view = new View(miniBotMap)
    val offsetS = Time("getRelPosForType S", {view.getRelPosForType('S')})
    assert(offsetS.length == 6)

    val offsetM = Time("getRelPosForType M", {view.getRelPosForType('M')})
    assert(offsetM.isEmpty)
  }

  // View time: 503.91900 nanos
  test("Performance test") {
    var time = 0.0
    var count = 0

    (1 to 1000000).foreach(_ => {
      time = time + Time.record({ new View(miniBotMap) })
      count += 1
    })

    println("View time: " + BigDecimal( time / count).setScale(5, BigDecimal.RoundingMode.HALF_UP) + " nanos")
  }

  test("Timing stuff") {
    val view = new View(miniBotMap)
    Time("cellAtRelPos", {view.cellAtRelPos(new XY(-4,8))})
    Time("indexFromRelPos", {view.indexFromRelPos(new XY(-4,8))})
    Time("indexFromAbsPos", {view.indexFromAbsPos(new XY(5,9))})
    Time("absPosFromRelPos", {view.absPosFromRelPos(new XY(-5,9))})
    Time("cellAtAbsPos", {view.cellAtAbsPos(new XY(3,3))})
    Time("cellAtAbsPos", {view.cellAtAbsPos(new XY(3,3))})
    Time("offsetToNearest", {view.offsetToNearest('s')})
    Time("offsetToNearestEnemy", {view.offsetToNearestEnemy()})
    Time("relPosFromIndex", {view.relPosFromIndex(6)})
    Time("relPosFromIndexFromOffset", {view.relPosFromIndexFromOffset(6, new XY(2,2))})
    Time("absPosFromIndex", {view.absPosFromIndex(2)})
    Time("relPosFromAbsPos", {view.relPosFromAbsPos(new XY(2,3))})
    Time("getRelPosForType", {view.getRelPosForType('s')})
    Time("countVisibleEnemies", {view.countVisibleEnemies()})
    Time("countType", {view.countType('s')})
  }


  val miniBotMap = "" +
    "__????Wp_______s_____" +
    "___?W?WB________s____" +
    "____W?W______________" +
    "______W_________S____" +
    "_____________________" +
    "_______________S_____" +
    "_____________________" +
    "_____________________" +
    "_____________________" +
    "______P_SS___________" +
    "_________SS__________" + // Center
    "_____________________" +
    "_____________________" +
    "__________________WWW" +
    "__________________W??" +
    "__________________W??" +
    "__________________W??" +
    "__________________W??" +
    "__________________W??" +
    "__________________WW?" +
    "_____P__p__________??"
}

