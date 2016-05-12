package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size(t: Tree[Int]): Int = {
    t match {
      case Branch(l, r) => size(l) + size(r) + 1
      case Leaf(value) => 1
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Branch(l, r) => maximum(l) max maximum(r)
      case Leaf(v) => v
    }
  }

  def depth[A](t: Tree[A]): Int = {
    def go(t: Tree[A], d: Int): Int = {
      t match {
        case Branch(l, r) => go(l, d + 1) max go(r, d + 1)
        case Leaf(v) => d
      }
    }
    go(t, 1)
  }
}