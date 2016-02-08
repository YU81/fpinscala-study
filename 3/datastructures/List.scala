package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //  def hoge(): Unit = {
  //    val x = List(1, 2, 3, 4, 5) match {
  //      case Cons(x, Cons(2, Cons(4, _))) => x
  //      case Nil => 42
  //      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  //      case Cons(h, t) => h + sum(t)
  //      case _ => 101
  //    }
  //  }

  def tail[A](lst: List[A]): List[A] = {
    lst match {
      case Nil => Nil
      case Cons(lst, lsta) => lsta
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    // FIXME : Cons と match を使おう！！
    //    def exec(l: List[A], n: Int, cnt: Int): List[A] = {
    //      if (cnt >= n) {
    //        return l
    //      }
    //      val ret = tail(l)
    //      exec(ret, n, cnt + 1)
    //    }
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def dropWhileFilter[A](x: A): Boolean = {
    x != 5
  }

  def dropWhileAnswer[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  // val list = fpinscala.datastructures.List(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
  // fpinscala.datastructures.List.dropWhile(list, fpinscala.datastructures.List.dropWhileFilter )

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def init[A](l: List[A]): List[A] = {
    // FIXME : appendを使わない！
    //    def go(l: List[A], head: List[A]): List[A] = {
    //      l match {
    //        case Cons(h, t) if t != Nil => go(t, fpinscala.datastructures.List.append(head, fpinscala.datastructures.List(h)))
    //        case _ => head
    //      }
    //    }
    //    go(l, fpinscala.datastructures.List())
    //
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def dropWhileCurry[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhileCurry(t)(f)
      case _ => l
    }
  }
}
