package org.brunoribeiro2k.fpinscala

class Chapter2 {

  /** Exercise 2.1: Fibonacci */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if (n <= 0) prev
      else loop(n - 1, cur, prev + cur)
    }
    loop(n, 0, 1)
  }

  /** Exercise 2.1 without tailrec */
  def fibNonTailrec(n: Int): Int = {
    n match {
      case x if x <= 0 => 0
      case 1 => 1
      case _ => fibNonTailrec(n - 1) + fibNonTailrec(n - 2)
    }
  }

  /** Exercise 2.2 */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      print(n)
      if (n >= as.length) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }

  /** Exercise 2.3 */
  def curry[A,B,C](f: (A, B) => C): A => B => C = {
    a => f(a, _)
  }

  /** Exercise 2.4 */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /** Exercise 2.5 */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
    // g andThen f
    // f compose g
  }

}