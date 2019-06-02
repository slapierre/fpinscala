package fpinscala.ch03.datastructures.e31

// EXERCISE 3.1, page 34

sealed trait MyList[+A] // `List` data type, parameterized on a type, `A`
case object MyNil extends MyList[Nothing] // A `List` data Constructor representing the empty list
/* Another data Constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `MyCons`.
 */
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: MyList[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case MyNil => 0 // The sum of the empty list is 0.
    case MyCons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def apply[A](as: A*): MyList[A] = // Variadic function syntax
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))
}

object MyTest extends App {

  override def main(args: Array[String]): Unit = {

    val x = MyList(1,2,3,4,5) match {
      case MyCons(x, MyCons(2, MyCons(4, _))) =>
        println ("case 1")
        x
      case MyNil =>
        println ("case 2")
        42
      case MyCons(x, MyCons(y, MyCons(3, MyCons(4, _)))) =>
        println ("*** case 3 ***")
        x + y
      case MyCons(h, t) =>
        println ("case 4")
        h + MyList.sum(t)
      case _ =>
        println ("case 5")
        101
    }

    println (s"Match result, x = ${x}")
  }

}
