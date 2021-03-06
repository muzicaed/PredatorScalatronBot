import org.scalatest.FunSuite
import utils._
import java.util

class BotTestSuite extends FunSuite {

  test("Should create bot instance") {
    val params = botParams()
    val bot = Time("new BotImpl", { new Bot(params, 3000) })
    assert(bot.energy == 350)
  }

  test("Should set and get string params") {
    val params = botParams()
    val bot = new Bot(params, 3000)

    val result = Time("inputOrElse", { bot.inputOrElse("str_data", "No data") })
    assert(result == "A test string")

    val result2 = Time("inputOrElse", { bot.inputOrElse("str_data_no", "No data") })
    assert(result2 == "No data")
  }

  test("Should set and get int params") {
    val params = botParams()
    val bot = new Bot(params, 3000)

    val result = Time("inputAsIntOrElse", { bot.inputAsIntOrElse("data", 0) })
    assert(result == 200)

    val result2 = Time("inputAsIntOrElse", { bot.inputAsIntOrElse("data_no", 0) })
    assert(result2 == 0)
  }

  test("Should set and get XY params") {
    val params = botParams()
    val bot = new Bot(params, 3000)

    val result = Time("inputAsXYOrElse", { bot.inputAsXYOrElse("xy_data", XY.Zero) })
    assert(result == new XY(5, -12))

    val result2 = Time("inputAsXYOrElse", { bot.inputAsXYOrElse("xy_data_no", XY.Zero) })
    assert(result2 == XY.Zero)
  }

  test("Should create response command string") {
    val params = botParams()
    val bot = new Bot(params, 3000)
    bot.move(new XY(1,1))
    bot.spawn(new XY(-1, -1), "energy" -> 100, "type" -> "test")
    bot.status("TEST")

    val result = Time("toString", {bot.toString()})
    assert(result == "Move(direction=1:1)|Spawn(direction=-1:-1,energy=100,type=test)|Status(text=TEST)")
  }

  // BotImpl time: 608.61760 nanos
  test("Performance test") {
    val params = botParams()
    var time = 0.0
    var count = 0

    (1 to 10000000).foreach(_ => {
      time = time + Time.record({ new Bot(params, 3000) })
      count += 1
    })

    println("BotImpl time: " + BigDecimal( time / count).setScale(5, BigDecimal.RoundingMode.HALF_UP) + " nanos")
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
    map.put("data", "200")
    map.put("str_data", "A test string")
    map.put("xy_data", new XY(5, -12).toString)
    map
  }

}

