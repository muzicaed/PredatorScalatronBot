import java.util

import org.scalatest.FunSuite
import utils._

class XYTestSuite extends FunSuite {

   test("Should perform stepCount & distance & length") {
     val test = new XY(4, 5)
     val stepCountResult = Time("stepCount", {test.stepCount})
     assert(stepCountResult == 5)

     val distanceToResult = Time("distanceTo", {test.distanceTo(XY.Zero)})
     assert(distanceToResult.toInt == 6)

     val length = Time("length", {test.length})
     assert(length.toInt == 6)
   }
  test("Should convert Rel XY to Direction45") {
    val rightUp = new XY(8, -7)
    val right = new XY(8, 0)
    val leftDown = new XY(-8, 5)

    val rightUpDir = Time("rightUp.toDirection45", {rightUp.toDirection45})
    val rightDir = Time("right.toDirection45", {right.toDirection45})
    val leftDownDir = Time("leftDown.toDirection45", {leftDown.toDirection45})

    assert(rightUpDir == Direction45.RightUp)
    assert(rightDir == Direction45.Right)
    assert(leftDownDir == Direction45.LeftDown)
  }


  // BotImpl time: 76.61760 nanos
  test("Performance test") {
    var time = 0.0
    var count = 0

    (1 to 10000000).foreach(i => {
      time = time + Time.record({ new XY(i, i) })
      count += 1
    })

    println("XY time: " + BigDecimal( time / count).setScale(5, BigDecimal.RoundingMode.HALF_UP) + " nanos")
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

