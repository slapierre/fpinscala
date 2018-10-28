package fpinscala.gettingstarted

// EXERCISE 2.4, page 27
// Implement uncurry , which reverses the transformation of curry . Note that since =>
// associates to the right, A => (B => C) can be written as A => B => C .
// def uncurry[A,B,C](f: A => B => C): (A, B) => C

// https://github.com/scala/scala/blob/v2.12.4/src/library/scala/Function.scala#L44

// Exercise 4: Implement `uncurry`
object MyUncurry {

  // Input: a function that takes an A and returns a function that takes a B and returns a C
  // Output: a function that takes 2 parameters and returns a
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  /*
   NB: There is a method on the `Function` object in the standard library,
   `Function.uncurried` that you can use for uncurrying.

   Note that we can go back and forth between the two forms. We can curry
   and uncurry and the two forms are in some sense "the same". In FP jargon,
   we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
   a term we inherit from category theory.
   */

  def curriedSum(x: Int)(y: Int): Int = x + y

  def main(args: Array[String]): Unit = {
    val u = uncurry(curriedSum)
    val v = u(3,4)
    println (s"Uncurried sum: ${v}")
  }
}
