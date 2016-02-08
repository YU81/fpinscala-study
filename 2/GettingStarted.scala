import scala.annotation.tailrec

object GettingStarted {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  //  private def formatAbs(x: Int) = {
  //    val msg = "The absolute value of %d is %d"
  //
  //    msg.format(x, abs(x))
  //  }

  def main(args: Array[String]): Unit =
  // TODO : printlnがまるでsortされたように出力される！！
    println(formatAbs(-42))

  println(factorial(20))
  println(factorial(20))
  println(formatAbs(-42))
  println(fib(1))

  // isSorted
  val ord: (String, String) => Boolean = (x, y) => x <= y
  val str1 = Array("a", "b", "c")
  val str2 = Array("2", "z", "r")

  println(isSorted[String](str1, ord)) // assertTrue
  println(isSorted[String](str2, ord)) // assertFalse
  println(isSorted[String](str1, (x, y) => x <= y))

  // assertFalse

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    def go(n: Int): Int =
      n match {
        case 0 => 0
        case 1 => 1
        case 2 => 1
        case _ => go(n - 1) + go(n - 2)
      }

    go(n)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d os %d"
    msg.format(name, n, f(n))
  }

  def formatAbs(x: Int) = {
    this.formatResult("absolute value", x, this.abs)
  }

  def formatFactorial(n: Int) = {
    this.formatResult("factorial", n, this.factorial)
  }

  //  def findFirst(ss: Array[String], key: String): Int = {
  //    def loop(n: Int): Int = {
  //      if (n >= ss.length) -1
  //      else if (ss(n) == key) n
  //      else loop(n + 1)
  //    }
  //    loop(0)
  //  }


  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) 1
      else loop(n + 1)
    }
    loop(0)
  }

  def isSorted() = {}

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) return true
      if (!ordered(as(n), as(n + 1))) return false
      loop(n + 1)
    }
    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }
}
