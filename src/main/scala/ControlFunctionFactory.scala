
/**
 * Entry point
 */
class ControlFunctionFactory {
  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "React" =>
        val bot = new BotImpl(params)
        if( bot.generation == 0 ) {
          bot.status("Predator")
          ControlFunction.forHunter(bot)
        } else {
          ControlFunction.forSlave(bot)
        }
        bot.toString
      case _ => "" // OK
    }
  }
}