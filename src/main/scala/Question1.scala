/**
  * 1. Two Sum
  *
  * Given an array of integers, return indices of the two numbers such that they add up to a specific target.
  *
  * You may assume that each input would have exactly one solution, and you may not use the same element twice.
  *
  * Example:
  *
  * Given nums = [2, 7, 11, 15], target = 9,
  *
  * Because nums[0] + nums[1] = 2 + 7 = 9,
  * return [0, 1].
  */
object Question1 {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    for ( x <- 0 until nums.length - 1){
      for ( y <- x + 1 until nums.length) {
        if (nums(x) + nums(y) == target) return Array(x,y)
      }
    }
    Array()
  }
}