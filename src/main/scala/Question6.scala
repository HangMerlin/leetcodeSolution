/**
  * The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this: (you may want to display this pattern in a fixed font for better legibility)
  *
  * P   A   H   N
  * A P L S I I G
  * Y   I   R
  * And then read line by line: "PAHNAPLSIIGYIR"
  *
  * Write the code that will take a string and make this conversion given a number of rows:
  *
  * string convert(string s, int numRows);
  * Example 1:
  *
  * Input: s = "PAYPALISHIRING", numRows = 3
  * Output: "PAHNAPLSIIGYIR"
  * Example 2:
  *
  * Input: s = "PAYPALISHIRING", numRows = 4
  * Output: "PINALSIGYAHRPI"
  * Explanation:
  *
  * P     I    N
  * A   L S  I G
  * Y A   H R
  * P     I
  */
object Question6 {
  def convert(s: String, numRows: Int): String = {
    if (numRows <= 1) s
    else {
      splitString(s,numRows)
        .map(t =>
          (subStringToListString(t._1,numRows) zip subStringToInverseListString(t._2,numRows))
            .map(concatChars)
            .map(_.trim))
        .foldLeft[Seq[String]](List.fill(numRows)(""))(concatStringSeq)
        .mkString("")
    }

  }

  def subStringToListString(str: String, numRows: Int): Seq[Char] = (str + (" " * (numRows - str.length))).toList

  def subStringToInverseListString(str: String, numRows: Int): Seq[Char] = (str + (" " * (numRows - str.length))).toList.reverse

  def splitString(str: String, numRows: Int): Seq[(String,String)] = str.grouped(numRows * 2 - 2).toSeq.map(s => s.splitAt(numRows - 1))

  def concatChars(t: (Char,Char)): String = t._1.toString + t._2

  def concatStringSeq(t1: Seq[String], t2: Seq[String]): Seq[String] = (t1 zip t2).map(s => s._1 + s._2)
}
