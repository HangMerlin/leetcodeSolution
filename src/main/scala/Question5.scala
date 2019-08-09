/**
 *5. Longest Palindromic Substring
 *
 * Given a string s, find the longest palindromic substring in s. You may assume that the maximum length of s is 1000.
 *
 * Example 1:
 *
 * Input: "babad"
 * Output: "bab"
 * Note: "aba" is also a valid answer.
 * Example 2:
 *
 * Input: "cbbd"
 * Output: "bb"
 */
object Question5 {
  def longestPalindrome(s: String): String = {
    if (isPalindrome(s)) s
    else getMaxPalindrome(s)
  }

  def getSubStringsWithDrop(s: String, numberDrop: Int): Stream[String] =
    for {
      i <- (0 to numberDrop).toStream
      r = s.drop(i).dropRight(numberDrop - i)
    } yield r

  //fixme still can be more efficient
  def getMaxPalindrome(s: String): String = {
    val len = s.length
    (1 to len).find(i => getSubStringsWithDrop(s, i).exists(isPalindrome)) match {
      case Some(in) => getSubStringsWithDrop(s, in).find(isPalindrome).get
      case None => ""
    }
  }

  def isPalindrome(s: String): Boolean = s.reverse == s
}
