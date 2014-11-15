import java.util

import org.scalatest.FunSuite
import utils._

class XYTestSuite extends FunSuite {

   test("Should perform stepCount & distance & length") {
     val test = XY(4, 5)
     val stepCountResult = Time("stepCount", {test.stepCount})
     assert(stepCountResult == 5)

     val distanceToResult = Time("distanceTo", {test.distanceTo(XY.Zero)})
     assert(distanceToResult.toInt == 6)

     val length = Time("length", {test.length})
     assert(length.toInt == 6)
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
     map.put("xy_data", XY(5, -12).toString)
     map
   }

 }

