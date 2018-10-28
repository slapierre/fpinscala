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
        case List(last) =>
          println ("go, list of 1 element: compare!")
          compare(lhs, last) // compare with last element
        case _ =>
          println ("go, list of many element: compare!")
          val elem = rhs.head
          if (!compare(lhs, elem))
            false
          else
            go(elem, rhs.tail)
      }
    }

    xs match {
      case Nil =>
        println ("empty list, sorted!")
        true
      case List(_) =>
        println ("list of 1 element, sorted!")
        true
      case _ =>
        println ("non-empty list, call go!")
        go(xs.head, xs.tail)
    }
  }

  def compareInt(lhs: Int, rhs: Int): Boolean = {
    println(s"compareInt, lhs: $lhs, rhs: $rhs, comparison: ${lhs <= rhs}")
    lhs <= rhs
  }

  // Call from REPL: MySort.main(Array())
  def main(args: Array[String]): Unit = {
    println(s"* Empty List: ${isSorted(List(), compareInt)}")
    println(s"* List with 1 element: ${isSorted(List(1), compareInt)}")
    println(s"* List with 2 elements (sorted: ${isSorted(List(1, 2), compareInt)}")
    println(s"* List with 2 elements (unsorted): ${isSorted(List(2, 1), compareInt)}")
    println(s"* List with 5 elements (sorted): ${isSorted(List(1, 2, 3, 4, 5), compareInt)}")
    println(s"* List with 5 elements (unsorted): ${isSorted(List(1, 2, 4, 3, 5), compareInt)}")
  }
}
