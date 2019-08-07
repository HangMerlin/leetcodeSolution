/**
  * There are two sorted arrays nums1 and nums2 of size m and n respectively.
  *
  * Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).
  *
  * You may assume nums1 and nums2 cannot be both empty.
  *
  * Example 1:
  *
  * nums1 = [1, 3]
  * nums2 = [2]
  *
  * The median is 2.0
  * Example 2:
  *
  * nums1 = [1, 2]
  * nums2 = [3, 4]
  *
  * The median is (2 + 3)/2 = 2.5
  */
object Question4 {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    val nums = (nums1 ++ nums2).sorted
    val size = nums.length
    size % 2 match {
      case 1 => nums(size/2)
      case 0 => (nums(size/2).toDouble + nums(size/2 - 1).toDouble)/2
    }
  }
}