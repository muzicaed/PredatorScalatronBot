package utils

/**
 * Times a code block (or function) execution.
 */
object Time {

  def apply[A](tag: String, code: => A) = {
    val startTime = System.nanoTime()
    val codeResult = code
    val endTime = System.nanoTime()
    println(tag + ": " + (startTime - endTime))
    codeResult
  }
}
