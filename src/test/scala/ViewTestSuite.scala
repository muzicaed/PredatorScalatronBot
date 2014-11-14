import org.scalatest.FunSuite
import utils.{XY, Time, View}

/**
 * Created by mikaelhellman on 14-11-15.
 */
class ViewTestSuite extends FunSuite {

  test("Should count visible enemies") {
    val view = View(miniBotMap)
    val count = Time("countVisibleEnemies", {view.countVisibleEnemies()})
    assert(count == 2)
  }

  test("Should give the distance to closest enemy") {
    val view = View(miniBotMap)
    val offset = Time("offsetToNearestEnemy", {view.offsetToNearestEnemy()})
    assert(offset == XY(6, -9))
  }

  test("Timing stuff") {
    val view = View(miniBotMap)
    Time("absPosFromIndex", {view.absPosFromIndex(8)})
    Time("relPosFromAbsPos", {view.relPosFromAbsPos(XY(6,4))})
    Time("getRelPosForType", {view.getRelPosForType('S')})
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

