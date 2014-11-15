import control._
import utils.{Time, BotImpl, CommandParser, SlaveType}

/**
 * Entry point
 */
class ControlFunctionFactory {

  var apocalypse = 0

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "Welcome" =>
        apocalypse = params("apocalypse").toInt
        ""

      case "React" =>
        val bot = new BotImpl(params, apocalypse)
        if (bot.generation == 0) {
          MasterControl(bot)
          apocalypse -= 2
        } else {
          bot.inputOrElse("type", SlaveType.INVALID) match {

            case SlaveType.HUNTER => HunterControl(bot)
            case SlaveType.VAMPIRE => Time("Vampire", {VampireControl(bot)})
            case SlaveType.MISSILE => MissileControl(bot)
            case SlaveType.DEFENCE => DefenceControl(bot)
            case SlaveType.SWARMER => SwarmerControl(bot)
            case SlaveType.DROP_BOMB => DropBombControl(bot)

          }
        }
        bot.toString

      case _ => "" // OK
    }
  }
}