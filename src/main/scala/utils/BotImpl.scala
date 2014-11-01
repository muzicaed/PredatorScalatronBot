package utils

/**
 * Bot implementation for both Bot and utils.MiniBot
 */
case class BotImpl(inputParams: Map[String, String], apocalypseIn: Int) extends MiniBot {
  // input
  def inputOrElse(key: String, fallback: String) = inputParams.getOrElse(key, fallback)
  def inputAsIntOrElse(key: String, fallback: Int) = inputParams.get(key).map(_.toInt).getOrElse(fallback)
  def inputAsXYOrElse(key: String, fallback: XY) = inputParams.get(key).map(s => XY(s)).getOrElse(fallback)

  val apocalypse = apocalypseIn
  val view = View(inputParams("view"))
  val energy = inputParams("energy").toInt
  val time = inputParams("time").toInt
  val generation = inputParams("generation").toInt
  def offsetToMaster = inputAsXYOrElse("master", XY.Zero)


  // output

  private var stateParams = Map.empty[String,Any]     // holds "Set()" commands
  private var commands = ""                           // holds all other commands
  private var debugOutput = ""                        // holds all "Log()" output

  /** Appends a new command to the command string; returns 'this' for fluent API. */
  private def append(s: String) : Bot = { commands += (if(commands.isEmpty) s else "|" + s); this }

  /** Renders commands and stateParams into a control function return string. */
  override def toString = {
    var result = commands
    if(!stateParams.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(",",",")")
    }
    if(!debugOutput.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += "Log(text=" + debugOutput + ")"
    }
    result
  }

  def log(text: String) = { debugOutput += text + "\n"; this }
  def move(direction: XY) = append("Move(direction=" + direction + ")")
  def say(text: String) = append("Say(text=" + text + ")")
  def status(text: String) = append("Status(text=" + text + ")")
  def explode(blastRadius: Int) = append("Explode(size=" + blastRadius + ")")
  def spawn(offset: XY, params: (String,Any)*) =
    append("Spawn(direction=" + offset +
      (if(params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
      ")")
  def set(params: (String,Any)*) = { stateParams ++= params; this }
  def set(keyPrefix: String, xy: XY) = { stateParams ++= List(keyPrefix+"x" -> xy.x, keyPrefix+"y" -> xy.y); this }
}
