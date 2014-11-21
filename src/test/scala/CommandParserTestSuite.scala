import org.scalatest.FunSuite
import utils.{CommandParser, Time}

class CommandParserTestSuite extends FunSuite {

  val testCommand = "React(generation=1,time=128,view=_________________PW??__________________WWW________________________________________________B___________________________________________________________________________WW_________________________s__________________________S_______W??_______________P__W??__________________W??___________________??______________________________________________________s__________________________________________________________b____________WWWW______________s__,master=30:-21,energy=596,slaves=7,target=5,type=Vampire,name=Slave_8,missileDelay=128)"

  test("Should parse command") {
    val (opcode, result) = CommandParser(testCommand)
    assert(opcode == "React")
    assert(result.get("time") == "128")
    assert(result.get("name") == "Slave_8")
    assert(result.get("slaves") == "7")
    assert(result.get("missileDelay") == "128")
    assert(result.get("type") == "Vampire")
    assert(result.get("generation") == "1")
    assert(result.get("energy") == "596")
    assert(result.get("master") == "30:-21")
  }

  test("Performance test") {
    var time = 0.0
    var count = 0

    (1 to 50000).foreach(_ => {
      time = time + Time.record({ CommandParser(testCommand) })
      count += 1
    })

    println("CommandParser time: " + time / count + " millis")
  }
}

