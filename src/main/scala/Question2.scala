/**
  * 2. Add Two Numbers
  *
  * You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order and each of their nodes contain a single digit. Add the two numbers and return it as a linked list.
  *
  * You may assume the two numbers do not contain any leading zero, except the number 0 itself.
  *
  * Example:
  *
  * Input: (2 -> 4 -> 3) + (5 -> 6 -> 4)
  * Output: 7 -> 0 -> 8
  * Explanation: 342 + 465 = 807.
  */
class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}

object Question2 {

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    (l1,l2) match {
      case (null,null) => null
      case (null,_) => l2
      case (_,null) => l1
      case (_,_) => createListNode(l1.x+l2.x, addTwoNumbers(l1.next,l2.next))
    }
  }
  def createListNode(x: Int, next: ListNode = null): ListNode = {
    val nextNode = if (x>=10) addTwoNumbers(new ListNode(1),next) else next
    val node = if (x>=10) new ListNode(x-10) else new ListNode(x)
    node.next=nextNode
    node
  }
}