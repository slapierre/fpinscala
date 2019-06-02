package fpinscala.ch02.gettingstarted

// EXERCISE 2.5, page 27
// Implement the higher-order function that composes two functions.
// def compose[A,B,C](f: B => C, g: A => B): A => C
// This is such a common thing to want to do that Scala’s standard library provides

// compose as a method on Function1 (the interface for functions that take one argu ment).
// To compose two functions f and g , we simply say f compose g .

object MyCompose {

  // Input: two parameters
  //   - param1, a function f that takes a B and return a C
  //   - param2, a function g that takes an A and return a B
  // Output: a function that takes an A and returns a C
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def compose[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D =
    (a: A) => f(g(h(a)))

  def compose_answer[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit = {
    val double = (x: Int) => x * 2
    val triple = (x: Int) => x * 3
    val negate = (x: Int) => x * -1

    val c1 = double andThen triple
    println (s"Using andThen, ${c1(1)} = 1*2*3")

    val c2 = compose(double, triple)
    println (s"Using compose (arity 2), ${c2(1)} = 1*2*3")

    val c3 = compose(double, triple, negate)
    println (s"Using compose (arity 3), ${c3(1)}")
  }
}
