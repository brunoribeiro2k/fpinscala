package org.brunoribeiro2k.fpinscala

class Chapter3Lists {

  /** Exercise 3.1 */
  /** Answer: 3 */

  /** Exercise 3.2 */
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case _ :: t => t
    }
  }

  /** Exercise 3.3 */
  def setHead[A](l: List[A], x: A): List[A] = {
    l match {
      case Nil => List(x)
      case xs => x :: xs
    }
  }

  /** Exercise 3.4 */
  @annotation.tailrec
  final def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (Nil, _) => Nil
      case (_, n) if n <= 0 => l
      case (h :: t, 1) => t
      case (h :: t, _) => drop(t, n - 1)
    }
  }

  /** Exercise 3.5 */
  @annotation.tailrec
  final def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case h :: t if !f(h) => h :: t
      case _ :: t => dropWhile(t, f)
    }
  }

  /** Exercise 3.6 */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case _ :: Nil => Nil
      case h :: t => h :: init(t)
    }
  }

  /**
   * Exercise 3.7
   * Answer: No, it would be necessary to replace the definition of the list by a Nil and pass the result 0.
   */
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case h :: t => f(h, foldRight(t, z)(f))
    }
  }
  def product(ns: List[Double]): Double = {
    foldRight(ns, 1.0)((x, y) => x * y)
  }

  /**
   * Exercise 3.8
   * scala> foldRight(List(1,2,3), Nil: List[Int])(_ :: _)
   * res12: List[Int] = List(1, 2, 3)
   */

  /** Exercise 3.9 */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  /** Exercise 3.10 */
  @annotation.tailrec
  final def foldLeft[A,B](as: List[A], acc: B)(f: (B, A) => B): B = {
    as match {
      case Nil => acc
      case h :: t => foldLeft(t, f(acc, h))(f)
    }
  }

  /** Exercise 3.11 */
  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((a, _) => a + 1)

  /** Exercise 3.12 */
  def reverse[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case h :: t => reverse(t) ++ List(h)
    }
  }
  def reverseLeft[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => y :: x)

  /** Exercise 3.13 */
  def foldLeftR[A,B](as: List[A], acc: B)(f: (B, A) => B): B = foldRight(as, acc)((x, y) => f(y, x))
  def sumLeftR(l: List[Int]): Int = foldLeftR(l, 0)(_ + _)
  def productLeftR(l: List[Double]): Double = foldLeftR(l, 1.0)(_ * _)
  def lengthLeftR[A](l: List[A]): Int = foldLeftR(l, 0)((a, _) => a + 1)
  def foldRightL[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((x, y) => f(y, x))
  def sumRightL(l: List[Int]): Int = foldRightL(l, 0)(_ + _)
  def productRightL(l: List[Double]): Double = foldRightL(l, 1.0)(_ * _)
  def lengthRightL[A](l: List[A]): Int = foldRightL(l, 0)((_, b) => 1 + b)

  /** Exercise 3.14 */
  def appendR[A](x: List[A], y: List[A]): List[A] = foldRightL(x, y)((a, b) => a :: b)
  def appendL[A](x: List[A], y: List[A]): List[A] = foldLeft(reverseLeft(x), y)((b, a) => a :: b)

  /** Exercise 3.15 */
  def concat[A](ls: List[List[A]]): List[A] = foldLeft(ls, Nil: List[A])(appendR)

  /** Exercise 3.16 */
  def increment(l: List[Int]): List[Int] = foldRightL(l, Nil: List[Int])((a, b) => a + 1 :: b)

  /** Exercise 3.17 */
  def listDoubleToString(l: List[Int]): List[String] = foldRight(l, Nil: List[String])((a, b) => a.toString :: b)

  /** Exercise 3.18 */
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, b) => f(a) :: b)

  /** Exercise 3.19 */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((h, t) => if (f(h)) h :: t else t)
  val lBig: List[Int] = List(1, 3, 9, 5, 7, 8, 6, 0, 4, 2, 80, 60, 0, 40, 20)
  filter(lBig)(_ % 2 == 0)

  /** Exercise 3.20 */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((a, b) => f(a) ++ b)

  /** Exercise 3.21 */
  def filterFM[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  /** Exercise 3.22 */
  def sumPairs(x: List[Int], y: List[Int]): List[Int] = {
    (x, y) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (h1 :: t1, h2 :: t2) => h1 + h2 :: sumPairs(t1, t2)
    }
  }

  /** Exercise 3.23  */
  def zipWith[A](x: List[A], y: List[A])(f: (A, A) => A): List[A] = {
    (x, y) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (h1 :: t1, h2 :: t2) => f(h1, h2) :: zipWith(t1, t2)(f)
    }
  }

  /** Exercise 3.24 */
  /** 1. startsWith: If a list xs starts with list ys, ys is a subsequence of xs. */
  /** 2. hasSubsequence */
  def startsWith[A](xs: List[A], ys: List[A]): Boolean = {
    (xs, ys) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (xh :: xt, yh :: yt) => xh == yh && startsWith(xt, yt)
    }
  }
  @annotation.tailrec
  final def hasSubsequence[A](xs: List[A], ys: List[A]): Boolean = {
    xs match {
      case Nil => ys == Nil
      case _ if startsWith(xs, ys) => true
      case xh :: xt => hasSubsequence(xt, ys)
    }
  }

}