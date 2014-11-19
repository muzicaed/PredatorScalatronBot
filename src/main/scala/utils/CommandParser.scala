package utils

import java.util

/** Utility methods for parsing strings containing a single command of the format
  * "Command(key=value,key=value,...)"
  */
object CommandParser {

  def apply(command: String): (String, util.HashMap[String, String]) = {
    val segments = command.split('(')
    if( segments.length != 2 )
      throw new IllegalStateException("invalid command: " + command)
    val opcode = segments(0)
    val params = segments(1).dropRight(1).split(',')

    val map = new util.HashMap[String, String]()
    var i = 0
    while (i < params.length) {
      val segments = params(i).split('=')
      val value = if(segments.length>=2) segments(1) else ""
      map.put(segments(0), value)
      i += 1
    }

    (opcode, map)
  }
}
