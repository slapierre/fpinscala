package fpinscala.gettingstarted

import scala.annotation.tailrec

// EXERCISE 2.2, page 24
// Implement isSorted , which checks whether an Array[A] is sorted according to a
// given comparison function

// Implementing Exercise 2-2 with List instead of Array
object MySort {

  def isSorted[A](xs: List[A], compare: (A,A) => Boolean): Boolean = {

    @tailrec
    def go(lhs: A, rhs: List[A]): Boolean = {
      rhs match {
        case List(last) => compare(lhs, last) // Compare last element
        case h :: t => // Pattern matching on lists: section 16.5 of Programming in Scala
          if (!compare(lhs, h))
            false
          else
            go(h, t)
      }
    }

    xs match {
      case Nil => true
      case List(_) => true
      case _ => go(xs.head, xs.tail)
    }
  }

  def isSorted_answer[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (gt(as(n), as(n+1))) false
      else go(n+1)

    go(0)
  }

  def compareInt(lhs: Int, rhs: Int): Boolean = lhs <= rhs
  def compareStr(lhs: String, rhs: String): Boolean = lhs.length <= rhs.length

  def main(args: Array[String]): Unit = {
    println(s"* Empty List: ${isSorted(List(), compareInt)}")
    println(s"* List of 1 element: ${isSorted(List(1), compareInt)}")

    println
    println(s"* List of 2 integers (sorted): ${isSorted(List(1, 2), compareInt)}")
    println(s"* List of 2 integers (unsorted): ${isSorted(List(2, 1), compareInt)}")
    println(s"* List of 5 integers (sorted): ${isSorted(List(1, 2, 3, 4, 5), compareInt)}")
    println(s"* List of 5 integers (unsorted): ${isSorted(List(1, 2, 4, 3, 5), compareInt)}")

    println
    println(s"* List of 3 strings (sorted): ${isSorted(List("foo", "bar", "foobar"), compareStr)}")
    println(s"* List of 3 strings (unsorted): ${isSorted(List("foo", "foobar", "bar"), compareStr)}")
  }
}
