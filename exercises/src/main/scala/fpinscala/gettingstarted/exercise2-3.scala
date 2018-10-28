package fpinscala.gettingstarted

// EXERCISE 2.3, page 27
// Let’s look at another example, currying, 9 which converts a function f of two arguments
// into a function of one argument that partially applies f . Here again there’s only one
// implementation that compiles. Write this implementation.
// def curry[A,B,C](f: (A, B) => C): A => (B => C)

// Programming in Scala, section 9.3: Currying (page 173)
// Scala for the Impatient, section 12.8: Currying (page 164)

// https://en.wikipedia.org/wiki/Currying
// Currying is the technique of translating the evaluation of a function that takes multiple arguments into evaluating a sequence of functions, each with a single argument

// Exercise 3: Implement `curry`.
object MyCurry {

  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation! Here's an example:

  // Input: (a: A, f: (A,B) => C) -- input has 2 parameters, an A and a function that takes 2 parameters and returns a C
  // Output: B => C               -- output is a function that takes a B and returns a C
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  // Input: (f: (A, B) => C) -- input is function that takes 2 parameters and return a C
  // Output: A => (B => C)   -- output is a function that takes an A and returns a function that takes a B and returns a C
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => partial1(a, f) // partial1 returns a function that takes a B and returns a C

  /* NB: The `Function2` trait has a `curried` method already, so if you wanted to
     cheat a little you could write the answer as f.curried */
  def curry_answer[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y
  def curriedSum(x: Int)(y: Int): Int = x + y

  def main(args: Array[String]): Unit = {

    val p = partial1(3, sum)
    println(s"Partial sum: ${p(4)}, p is a `${p.getClass}`")

    val c = curry(sum)
    val v = c(1)(2)
    println (s"Currying sum: $v, c is a `${v.getClass}`")

    val cs = curriedSum(3)(4)
    println(s"Curried sum: $v, v is a `${cs.getClass}`")

    val t = curriedSum(3)_ // the underscore is a placeholder for second parameter
    println(s"Another Curried sum: ${t(4)}, t is a `${t.getClass}`")
  }
}
