import java.util

import analyzers.ExplosionAnalyzer
import org.scalatest.FunSuite
import utils._


class ExplosionAnalyserTestSuite extends FunSuite {

  test("Should calculate most effective blast radius") {
    val params = botParams()
    val bot = new Bot(params, 3000)
    ExplosionAnalyzer(bot, 100)
    assert(ExplosionAnalyzer.bestRadius == 3)
    assert(ExplosionAnalyzer.bestDamage == 610)

    val bot2 = new Bot(params, 3000)
    ExplosionAnalyzer(bot2, bot2.energy)
    assert(ExplosionAnalyzer.bestRadius == 7)
    assert(ExplosionAnalyzer.bestDamage == 828)
  }

  test("Should simulate explosion and calculate score") {

  }

  test("Performance test of ExplosionAnalyzer") {
    val params = botParams()
    val bot = new Bot(params, 3000)
    var time = 0.0

    (1 to 10000).foreach(_ => {
      time = time + Time.record({ ExplosionAnalyzer(bot, 100) })
    })

    println("ExplosionAnalyzer: " + time / 10000 + " millis")
  }

  def botParams(): util.HashMap[String, String] = {
    val map = new util.HashMap[String, String]()
    map.put("view", miniBotMap)
    map.put("energy", "350")
    map.put("time", "2000")
    map.put("generation", "1")
    map.put("slaves", "3")
    map
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
}

