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

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(l: List[Int]) = {
    foldRight(l, 0)((x, y) => x + y)
  }

  def product2(l: List[Double]) = {
    foldRight(l, 1.0)((x, y) => x * y)
  }

  def length[A](l: List[A]): Int = {
    // use foldRight
    def go[A](l: List[A], cnt: Int): Int = {
      l match {
        case Nil => 0
        case Cons(x, xs) => foldRight(l, 0)((_, cnt) => cnt + 1)
      }
    }
    go(l, 0)
  }

  def lengthAnswer[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
    }
  }

  /*
It's common practice to annotate functions you expect to be tail-recursive with the `tailrec` annotation. If the
function is not tail-recursive, it will yield a compile error, rather than silently compiling the code and resulting
in greater stack space usage at runtime.
*/
  //  @annotation.tailrec
  def foldLeftAnswer[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sumFoldLeft(l: List[Int]) = {
    foldLeft(l, 0)((x, y) => x + y)
  }

  def productFoldLeft(l: List[Double]) = {
    foldLeft(l, 1.0)((x, y) => x * y)
  }

  def reverse[A](l: List[A]): List[A] = {
    var ret = List()
    l match {
      case Nil => Nil
    }
  }
}