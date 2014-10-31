package utils

/**
 * Represents a Bot (Master)
 */
trait Bot {
  // inputs
  def inputOrElse(key: String, fallback: String): String
  def inputAsIntOrElse(key: String, fallback: Int): Int
  def inputAsXYOrElse(keyPrefix: String, fallback: XY): XY
  def view: View
  def energy: Int
  def time: Int
  def generation: Int

  // outputs
  def move(delta: XY) : Bot
  def say(text: String) : Bot
  def status(text: String) : Bot
  def spawn(offset: XY, params: (String,Any)*) : Bot
  def set(params: (String,Any)*) : Bot
  def log(text: String) : Bot
}
