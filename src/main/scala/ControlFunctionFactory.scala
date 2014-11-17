import control._
import utils.{BotImpl, CommandParser, SlaveType}

/**
 * Entry point
 */
class ControlFunctionFactory {

  var apocalypse = 0

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "Welcome" =>
        apocalypse = params.get("apocalypse").toInt
        ""

      case "React" =>
        var returnCommand = ""
        if (params.get("generation").toInt == 0 || params.get("energy").toInt > 0) {
          val bot = new BotImpl(params, apocalypse)
          if (bot.generation == 0) {
            MasterControl(bot)
            apocalypse -= 2
          } else {
            val slaveType = bot.inputOrElse("type", SlaveType.INVALID)
            if (slaveType == SlaveType.VAMPIRE) VampireControl(bot)
            else if (slaveType == SlaveType.MISSILE) MissileControl(bot)
            else if (slaveType == SlaveType.DEFENCE) DefenceControl(bot)
            else if (slaveType == SlaveType.DROP_BOMB) DropBombControl(bot)
          }
          returnCommand = bot.toString
        }

        returnCommand

      case _ => "" // OK
    }
  }
}