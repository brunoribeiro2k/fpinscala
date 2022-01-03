package org.brunoribeiro2k.fpinscala

import scala.{Option => _, Some => _, None => _}

sealed trait Option[+A] {
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  /** Exercise 4.1 */
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case _ => None
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    // this match {
    //   case Some(a) => f(a)
    //   case _ => None
    // }
    this.map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(a) => a
      case _ => default
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    //this match {
    //  case Some(_) => this
    //  case _ => ob
    //}
    // .map(Some(_)) => Return this
    // .getOrElse => if this is None, return ob
    this.map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    // this match {
    //   case Some(a) if f(a) => Some(a)
    //   case _ => None
    // }
    // flatMap -- why not map?
    // because the inner function (a => ...) returns an option
    this.flatMap(a => if (f(a)) Some(a) else None)
  }

}

object Option extends Option[Unit] {

  /** Exercise 4.2 */
  def mean(xs: Seq[Double]): Option[Double] = {
    xs match {
      case Nil => None
      case _ => Some(xs.sum/xs.length)
    }
  }
  def variance(xs: Seq[Double]): Option[Double] = {
    // val m = mean(xs)
    // mean(xs.map(x => math.pow(x - m, 2)))
    // how to use flatMap?
    // mean returns an Option, so we can apply an Option function over it
    // since the inner function (variance mean) returns an Option, we have to use flatMap outside, instead of map
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = {
    // lift: (f: A => B) => (g: Option[A] => Option[B])
    // g: (Option[A] => Option[B]) = lift(f)
    // let x be whatever g will receive as an argument Option[A]
    // apply map to x using f: A => B, returning Option[B]
    x => x.map(f)
  }

  /** Exercise 4.3 */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    // Assume a = Some(x), b = Some(y)
    // We want to apply f(x, y) and return Some(return)
    // (a, b) match {
    //   case (None, _) => None
    //   case (_, None) => None
    //   case (Some(x), Some(y)) => Some(f(x, y))
    // }
    // curry(f: (A, B) => C): A => B => C
    // We can apply "for a = Some(x), a.flatMap(for b = Some(y), b.map(f(x,y)))
    a.flatMap(x => b.map(y => f(x, y)))
  }

  def map22[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  /** Exercise 4.4 */
  /**
   * sequence(List(Some(1), Some(2))
   * sequence(Some(1) :: List(Some(2)))
   * Some(1).flatMap(h1 => sequence(List(Some(2))).map(t2 => h1 :: t2))
   * Some(1).flatMap(h1 => sequence(Some(2) :: Nil).map(t2 => h1 :: t2))
   * Some(1).flatMap(h1 => Some(2).flatMap(h2 => sequence(Nil).map(t2 => h2 :: t2)).map(t2 => h1 :: t2))
   * Some(1).flatMap(h1 => Some(2).flatMap(h2 => Some(Nil).map(t2 => h2 :: t2)).map(t2 => h1 :: t2))
   * Some(1).flatMap(h1 => Some(2).flatMap(h2 => Some(List(h2))).map(t2 => h1 :: t2))
   * Some(1).flatMap(h1 => Some(List(2)).map(t2 => h1 :: t2))
   * Some(1).flatMap(h1 => Some(h1 :: List(2)))
   * Some(1 :: List(2))
   * Some(List(1, 2))
   */
  /**
   * sequence(List(Some(1), None)
   * sequence(List(Some(1) :: List(None))
   * Some(1).flatMap(h1 => sequence(List(None)).map(t1 => h1 :: t1)
   * Some(1).flatMap(h1 => sequence(None :: Nil).map(t1 => h1 :: t1)
   * Some(1).flatMap(h1 => None.flatMap(h2 => sequence(Nil).map(t2 => h2 :: t2)).map(t1 => h1 :: t1))
   * Some(1).flatMap(h1 => None.flatMap(h2 => Some(Nil).map(t2 => h2 :: t2)).map(t1 => h1 :: t1))
   * Some(1).flatMap(h1 => None.flatMap(h2 => List(h2)).map(t1 => h1 :: t1))
   * Some(1).flatMap(h1 => None)
   * None
   */
  def sequence[A](l: List[Option[A]]): Option[List[A]] = {
    l match {
      case Nil => Some(Nil: List[A])
      case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))
    }
  }

  /** Exercise 4.5 */
  def traverse[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] = {
    l match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))((hh, tt) => hh :: tt)
    }
  }

}

