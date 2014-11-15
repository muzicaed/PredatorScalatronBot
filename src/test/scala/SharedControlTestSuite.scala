import control.SharedControl
import org.scalatest.FunSuite
import utils._


class SharedControlTestSuite extends FunSuite {

  test("Should convert best direction into XY") {
    val bot = new BotImpl(botParams, 3000)
    val directionValue = Array[Double](2.0, 4.0, 2.2, 23.2, 25.1, 23.1, 10.0)

    val bestDirection = Time("convertDirectionValueIntoMove", {SharedControl.convertDirectionValueIntoMove(bot, directionValue)})
    assert(bestDirection == XY(-1,0))
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

  def botParams = Map("slaves" -> "3", "generation" -> "1", "time" -> "2000", "view" -> miniBotMap, "energy" -> "350")
}

