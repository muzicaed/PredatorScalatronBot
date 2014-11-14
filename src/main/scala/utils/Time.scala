package utils

/**
 * Times a code block execution.
 */
object Time {

  def apply[A](tag: String, code: => A) = {
    val startTime = System.nanoTime()
    val codeResult = code
    val endTime = System.nanoTime()
    println(s"$tag: ${((endTime - startTime).toFloat / 1000000.0)} millis")
    codeResult
  }
}
