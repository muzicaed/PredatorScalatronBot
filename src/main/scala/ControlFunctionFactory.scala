import control._
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
          MasterControl(bot)
        } else {
          bot.inputOrElse("type", "invalid") match {
            case "Vampire" => {
              VampireControl(bot)
            }
            case "Missile" => {
              MissileControl(bot)
            }
            case "Defence" => {
              DefenceControl(bot)
            }
            case "Swarmer" => {
              SwarmerControl(bot)
            }
          }
        }

        bot.toString

      case _ => "" // OK
    }
  }
}