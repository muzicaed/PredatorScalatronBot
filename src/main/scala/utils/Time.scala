package utils

/**
 * Times a code block execution.
 */
object Time {

  def apply[B](tag: String, codeBlock: => B) = {
    val startTime = System.nanoTime()
    val codeResult = codeBlock
    val endTime = System.nanoTime()
    println(s"$tag: ${(endTime - startTime).toFloat / 1000000.0} millis")
    codeResult
  }

  def record[B](codeBlock: => B): Double = {
    val startTime = System.nanoTime()
    codeBlock
    val endTime = System.nanoTime()
    (endTime - startTime).toFloat / 1000000.0
  }
}
