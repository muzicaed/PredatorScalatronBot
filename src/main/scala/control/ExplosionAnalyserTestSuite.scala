package control

import analyzers.ExplosionAnalyzer
import org.scalatest.FunSuite
import utils._


class ExplosionAnalyserTestSuite extends FunSuite {

  test("Should calculate most effective blast radius") {
    val bot = new BotImpl(botParams, 3000)
    val radiusAndDamage = Time("ExplosionAnalyzer", { ExplosionAnalyzer(bot, bot.energy) })
    assert(radiusAndDamage._1 == 6)
    assert(radiusAndDamage._2 == 200)
  }


  def botParams = Map("slaves" -> "3", "generation" -> "1", "time" -> "2000", "view" -> miniBotMap, "energy" -> "350")

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
}

