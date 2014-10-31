import control.{MasterControl, MissileControl}
import utils.{BotImpl, CommandParser}

/**
 * Entry point
 */
class ControlFunctionFactory {
  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "Welcome" => ""

      case "React" =>
        val bot = new BotImpl(params)
        if (bot.generation == 0) {
          bot.status("Predator")
          MasterControl(bot)
        } else {
          bot.inputOrElse("type", "invalid") match {
            case "Vampire" => {
              bot.status("Vampire")
              MasterControl(bot)
            }

            case "Missile" => {
              bot.status("M")
              MissileControl(bot)
            }
          }
        }

        bot.toString

      case _ => "" // OK
    }
  }
}