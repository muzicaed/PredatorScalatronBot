import java.util

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
    opcode match {
      case "Welcome" =>
        apocalypse = params.get("apocalypse").toInt
        ""

      case "React" =>
        if (params.get("generation").toInt == 0 || !checkUselessBot(params)) {
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
        else if (params.get("energy").toInt > 0) "Explode(size=10)"
        else ""

      case _ => "" // OK
    }
  }

  /**
   * Checks if bot is useless (low on energy)
   */
  def checkUselessBot(params: util.HashMap[String, String]): Boolean = {
    val threshold =
      if (params.get("slaves").toInt > Const.UPPER_SPAWN_LIMIT) Const.USELESS_THRESHOLD * 3
      else if (params.get("slaves").toInt >= (Const.LOWER_SPAWN_LIMIT * 0.5)) Const.USELESS_THRESHOLD
      else 1

      params.get("energy").toInt < threshold
  }
}