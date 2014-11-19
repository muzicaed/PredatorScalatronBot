import control._
import utils.{BotImpl, CommandParser, Const, SlaveType}

/**
 * Entry point
 */
class ControlFunctionFactory {

  var apocalypse = 0

  /**
   * Main responder
   */
  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)

    if (opcode == "Welcome") {
      apocalypse = params.get("apocalypse").toInt
      ""

    } else if (opcode == "React") {
      if (params.get("generation").toInt == 0 || params.get("energy").toInt > Const.USELESS_THRESHOLD) {
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
        bot.toString
      }
      else ""

    }
    else ""
  }
}