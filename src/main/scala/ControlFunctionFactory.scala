import control._
import utils.{BotImpl, CommandParser}

/**
 * Entry point
 */
class ControlFunctionFactory {

  var apocalypse = 0

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "Welcome" => ""
        apocalypse = params("apocalypse").toInt

      case "React" =>
        val bot = new BotImpl(params, apocalypse)
        if (bot.generation == 0) {
          MasterControl(bot)
          apocalypse -= 2
        } else {
          bot.inputOrElse("type", "invalid") match {
            case "Hunter" => {
              HunterControl(bot)
            }
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
            case "DropBomb" => {
              DropBombControl(bot)
            }
          }
        }

        bot.toString

      case _ => "" // OK
    }
  }
}