package fpinscala.gettingstarted

// EXERCISE 2.1, page 21
// Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
// The first two Fibonacci numbers are 0 and 1 . The nth number is always the sum of the
// previous two—the sequence begins 0, 1, 1, 2, 3, 5 . Your definition should use a
// local tail-recursive function.

object MyFibonacci {

  // x0=0, x1=1, x2=1, x3=2, x4=3, x5=5, x6=8, x7=13, x8=21
  def fib(xf: Int): Int = {
    def go(x: Int, prev: Int, acc: Int): Int = {
      if (x == xf)
        acc
      else
        go(x + 1, acc, prev + acc)
    }

    // Remark: fib(0) and fib(1) are special cases
    xf match {
      case 0 => 0
      case 1 => 1
      case _ => go(2, 1, 1)
    }
  }

  // test implementation of `fib`
  def main(args: Array[String]): Unit = {
    println("Expected: 0, 1, 1, 2, 3, 5, 8")
    println("Actual:   %d, %d, %d, %d, %d, %d, %d".format(fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6)))

    val r = List.range(0, 20)
    println(r.map(fib))
  }
}
