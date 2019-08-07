/**
  * 3. Longest Substring Without Repeating Characters
  *
  * Given a string, find the length of the longest substring without repeating characters.
  *
  * Example 1:
  *
  * Input: "abcabcbb"
  * Output: 3
  * Explanation: The answer is "abc", with the length of 3.
  * Example 2:
  *
  * Input: "bbbbb"
  * Output: 1
  * Explanation: The answer is "b", with the length of 1.
  * Example 3:
  *
  * Input: "pwwkew"
  * Output: 3
  * Explanation: The answer is "wke", with the length of 3.
  * Note that the answer must be a substring, "pwke" is a subsequence and not a substring.
  */
object Question3 {
  def lengthOfLongestSubstring(s: String): Int = {
    if (s.length <= 1) s.length
    else calculWithRef(s.tail, 1, s.head.toString)
  }

  def calculWithRef(str: String, maxLength: Int, maxSubString: String): Int = {
    if (maxLength >= (str.length + maxSubString.length)) maxLength
    else if (str.length == 0)
      if (maxSubString.length > maxLength) maxSubString.length
      else maxLength
    else {
      val head : String = str.headOption match {
        case Some(c) => c.toString
        case None => ""
      }
      val tail : String = str.tail
      val currentMaxLength = maxSubString.length
      if (maxSubString.contains(head)) {
        val subSubString : String = maxSubString.split(head.toString).toList match {
          case _::b::Nil => b
          case _ => ""
        }
        if (currentMaxLength > maxLength)
          calculWithRef(tail, currentMaxLength, subSubString + head)
        else calculWithRef(tail, maxLength, subSubString + head)
      }
      else
        calculWithRef(tail, maxLength, maxSubString + head)
    }
  }
}
