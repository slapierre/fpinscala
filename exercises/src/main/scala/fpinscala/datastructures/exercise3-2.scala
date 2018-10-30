package fpinscala.datastructures.e32 // prevent clash with other exercises 

// EXERCISE 3.2 to , page 35

sealed trait MyList[+A] // `MyList` data type, parameterized on a type, `A`
case object MyNil extends MyList[Nothing] // A `MyList` data Constructor representing the empty List
// Another data Constructor, representing nonempty Lists. Note that `tail` is another `MyList[A]`,
// which may be `MyNil` or another `MyCons`.a
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A] 

object MyList { // `MyList` companion object. Contains functions for creating and working with MyLists.

  def sum(ints: MyList[Int]): Int = ints match { // A function that uses pattern matching to add up a MyList of integers
    case MyNil => 0 // The sum of the empty MyList is 0.
    case MyCons(x,xs) => x + sum(xs) // The sum of a MyList starting with `x` is `x` plus the sum of the rest of the MyList.
  }

  def product(ds: MyList[Double]): Double = ds match {
    case MyNil => 1.0
    case MyCons(0.0, _) => 0.0
    case MyCons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] = // Variadic function syntax
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
      case MyNil => a2
      case MyCons(h,t) => MyCons(h, append(t, a2))
    }

  def foldRight[A,B](as: MyList[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case MyNil => z
      case MyCons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: MyList[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: MyList[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // EXERCISE 3.2, page 35
  // Implement the function tail for removing the first element of a List.
  // Note that the function takes constant time.
  def tail[A](l: MyList[A]): MyList[A] = l match {
    case MyCons(h, t) => t
    case MyNil => MyNil
  }

  //  EXERCISE 3.3, page 36
  // Using the same idea, implement the function setHead for replacing the first element
  // of a List with a different value.
  def setHead[A](l: MyList[A], h: A): MyList[A] = l match {
    case MyCons(_, t) => MyCons(h, t) // Replace head
    case MyNil => MyCons(h, MyNil) // Create head
  }
  
  def drop[A](l: MyList[A], n: Int): MyList[A] = ???

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = ???

  def init[A](l: MyList[A]): MyList[A] = ???

  def length[A](l: MyList[A]): Int = ???

  def foldLeft[A,B](l: MyList[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: MyList[A])(f: A => B): MyList[B] = ???
  
  def toString[A](as:MyList[A]) = foldRight(as, "")((x,y) => x + "," + y) // TODO: remove trailing comma
}

object MyTest {

  def testTail() = {
    println ("Exercise 3.2")
    
    val e: MyList[String] = MyList()
	  val l = MyList("A","B","C")
    val t = MyList.tail(l)
    val u = MyList.tail(t)
    val v = MyList.tail(u)

    println (s"Tail of empty list, e: $e -> ${MyList.toString(e)}")
    println (s"Content of list, l: $l -> ${MyList.toString(l)}")
    println (s"Content of tail, t: $t -> ${MyList.toString(t)}")
    println (s"Content of tail, u: $u -> ${MyList.toString(u)}")
    println (s"Content of tail, v: $v -> ${MyList.toString(v)}")
  }
  
  def testSetHead = {
    println ("Exercise 3.3")
    
    val e = MyList()
    val l1 = MyList.setHead(e, "Z")
    val l2 = MyList("A","B","C")
    val l3 = MyList.setHead(l2, "Z")

    println (s"Empty list: e: $e -> ${MyList.toString(e)}")
    println (s"Set head of empty list, l1: $l1 -> ${MyList.toString(l1)}")
    println (s"Non-empty list: l2: $l2 -> ${MyList.toString(l2)}")
    println (s"Set head of non-empty list: l3: $l3 -> ${MyList.toString(l3)}")
  }

  def main(args: Array[String]): Unit = {
//    testTail
    testSetHead
  }    
}