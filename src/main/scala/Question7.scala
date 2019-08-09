/**
  * 7. Reverse Integer
  *
  * Given a 32-bit signed integer, reverse digits of an integer.
  *
  * Example 1:
  *
  * Input: 123
  * Output: 321
  * Example 2:
  *
  * Input: -123
  * Output: -321
  * Example 3:
  *
  * Input: 120
  * Output: 21
  *
  * Note:
  * Assume we are dealing with an environment which could only store integers within the 32-bit signed integer
  * range: [−231,  231 − 1].
  * For the purpose of this problem, assume that your function returns 0 when the reversed integer overflows.
  */
object Question7 {

  import scala.util.{Try, Success, Failure}

  def reverse(x: Int): Int = {
    val str = x.toString().toList
    val (maybeMinus, numberPart) = str.head match {
      case '-' => ("-", str.tail)
      case _ => ("", str)
    }

    val reversedNumber = Try {
      numberPart.reverse.mkString("").toInt
    } match {
      case Success(i) => i
      case Failure(_) => 0
    }

    (maybeMinus + reversedNumber).toInt
  }
}