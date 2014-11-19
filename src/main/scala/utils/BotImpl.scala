package utils

import java.util

/**
 * Bot implementation for both Bot and utils.MiniBot
 */
case class BotImpl(inputParams: util.HashMap[String, String], apocalypseIn: Int) extends MiniBot {
  // input
  def inputOrElse(key: String, fallback: String): String = {
    val value = inputParams.get(key)
    if (value == null) return fallback
    value
  }

  def inputAsIntOrElse(key: String, fallback: Int): Int = {
    val value = inputParams.get(key)
    if (value == null) return fallback
    value.toInt
  }

  def inputAsXYOrElse(key: String, fallback: XY): XY = {
    val value = inputParams.get(key)
    if (value == null) return fallback
    XY(value)
  }


  val apocalypse = apocalypseIn
  val view = View(inputParams.get("view"))
  val energy = inputParams.get("energy").toInt
  val time = inputParams.get("time").toInt
  val generation = inputParams.get("generation").toInt
  val slaves = inputParams.get("slaves").toInt
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
    if(stateParams.nonEmpty) {
      if(!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(",",",")")
    }
    if(debugOutput.nonEmpty) {
      if(!result.isEmpty) result += "|"
      result += "Log(text=" + debugOutput + ")"
    }
    result
  }

  def log(text: String) = { debugOutput += text + "\n"; this }
  def move(direction: XY) = append("Move(direction=" + direction + ")")
  def say(text: String) = append("Say(text=" + text + ")")//append("Say(text=)")
  def status(text: String) = append("Status(text=" + text + ")")//append("Status(text=)")
  def explode(blastRadius: Int) = append("Explode(size=" + blastRadius + ")")
  def spawn(offset: XY, params: (String,Any)*) =
    append("Spawn(direction=" + offset +
      (if(params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
      ")")
  def set(params: (String,Any)*) = { stateParams ++= params; this }
  def set(keyPrefix: String, xy: XY) = { stateParams ++= List(keyPrefix+"x" -> xy.x, keyPrefix+"y" -> xy.y); this }
}
