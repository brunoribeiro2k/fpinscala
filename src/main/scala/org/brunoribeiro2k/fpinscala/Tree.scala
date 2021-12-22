package org.brunoribeiro2k.fpinscala

sealed trait Tree[+A] {

  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  /** Exercise 3.25 */
  def size(): Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size() + r.size()
  }

  /** Exercise 3.27 */
  def depth(): Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.depth max r.depth
  }

  /** Exercise 3.28 */
  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  /** Exercise 3.29 */
  def fold[B](f: A => B, g: (B, B) => B): B = this match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  }
  def sizeF(): Int = this.fold(_ => 1, 1 + _ + _)
  def depthF(): Int = this.fold(_ => 1, (x, y) => 1 + (x max y))
  def mapF[B](f: A => B, g: (B, B) => B): B = this.fold(f, g(_, _))
}

object Tree extends Tree[Int] {

  /** Exercise 3.26 */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /** Exercise 3.29 */
  def maximumF(t: Tree[Int]): Int = t.fold(x => x, (x, y) => x max y)


}



