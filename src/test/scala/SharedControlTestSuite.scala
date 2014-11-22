import java.util

import control.SharedControl
import org.scalatest.FunSuite
import utils._


class SharedControlTestSuite extends FunSuite {

  test("Should convert best direction into XY") {
    val params = botParams()
    val bot = new Bot(params, 3000)
    val directionValue = Array[Double](2.0, 4.0, 2.2, 23.2, 25.1, 23.1, 10.0)

    val bestDirection = Time("convertDirectionValueIntoMove", {SharedControl.convertDirectionValueIntoMove(bot, directionValue)})
    assert(bestDirection == new XY(-1,0))
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
    "______P_Ss___________" +
    "____b____Sm__________" + // Center
    "_______b_____________" +
    "_____________________" +
    "__________________WWW" +
    "__________________W??" +
    "__________________W??" +
    "__________________W??" +
    "__________________W??" +
    "__________________W??" +
    "__________________WW?" +
    "_____P__p__________??"

  def botParams(): util.HashMap[String, String] = {
    val map = new util.HashMap[String, String]()
    map.put("view", miniBotMap)
    map.put("energy", "350")
    map.put("time", "2000")
    map.put("generation", "1")
    map.put("slaves", "3")
    map
  }
}

