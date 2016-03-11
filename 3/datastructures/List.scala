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
  def foldLeftByFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): A = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(foldRight(xs, z)(f), x)
    }
  }

  // ans 3.13
  /*
The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflows when implementing a strict `foldRight` function as we've done in this chapter. (We'll revisit this in a later chapter, when we discuss laziness).

The other implementations build up a chain of functions which, when called, results in the operations being performed with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B => B`, then calling the built up function with the `z` argument. Try expanding the definitions by substituting equals for equals using a simple example, like `foldLeft(List(1,2,3), 0)(_ + _)` if this isn't clear. Note these implementations are more of theoretical interest - they aren't stack-safe and won't work for large lists.
*/
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  // Here is the same function with much more description
  def foldLeftViaFoldRight_1[A, B](as: List[A], outerIdent: B)(combiner: (B, A) => B): B = {

    // foldLeft processes items in the reverse order from foldRight.  It's
    // cheating to use reverse() here because that's implemented in terms of
    // foldLeft!  Instead, wrap each operation in a simple identity function to
    // delay evaluation until later and stack (nest) the functions so that the
    // order of application can be reversed.  We'll call the type of this
    // particular identity/delay function BtoB so we aren't writing B => B
    // everywhere:
    type BtoB = B => B

    // Here we declare a simple instance of BtoB according to the above
    // description.  This function will be the identity value for the inner
    // foldRight.
    def innerIdent: BtoB = (b: B) => b

    // For each item in the 'as' list (the 'a' parameter below), make a new
    // delay function which will use the combiner function (passed in above)
    // when it is evaluated later.  Each new function becomes the input to the
    // previous function (delayFunc).
    //
    //                        This much is just the type signature
    //                  ,-------^-------.
    def combinerDelayer: (A, BtoB) => BtoB =
      (a: A, delayFunc: BtoB) => (b: B) => delayFunc(combiner(b, a))
    // `----------v---------'    `----------------v---------------'
    //         Paramaters                 The returned function

    // Pass the original list 'as', plus the simple identity function and the
    // new combinerDelayer to foldRight.  This will create the functions for
    // delayed evaluation with an combiner inside each one, but will not
    // apply any of those functions.
    def go: BtoB = foldRight(as, innerIdent)(combinerDelayer)

    // This forces all the evaluations to take place
    go(outerIdent)
  }

  /*
  Here is a sample run for further illustration purposes
  foldLeftViaFoldRight_1(List(1,2,3), Nil:List[Int])((b:B, a:A) => Cons(a, b))

  Becomes:
  foldRight(List(1,2,3), innerIdent)(combinerDelayer)(Nil)

  Becomes:
  def delay3(b:B) = innerIdent(combiner(b,  3))
  def delay2(b:B) = delay3(combiner(b, 2))
  def delay1(b:B) = delay2(combiner(b,  1))
  delay1(Nil)

  Becomes:
  def delay3(b:B) = innerIdent(combiner(b,  3))
  def delay2(b:B) = delay3(combiner(b, 2))
  delay2(combiner(Nil,  1))

  Becomes:
  def delay3(b:B) = innerIdent(combiner(b,  3))
  delay3(combiner(combiner(Nil,  1), 2))

  Becomes:
  innerIdent(combiner(combiner(combiner(Nil,  1), 2),  3))

  Becomes:
  combiner(combiner(combiner(Nil,  1), 2),  3)

  Becomes:
  combiner(combiner(Cons(1,Nil), 2),  3)

  Becomes:
  combiner(Cons(2,Cons(1,Nil)),  3)

  Becomes:
  Cons(3,Cons(2,Cons(1,Nil)))
  Voila!



  Alternate Route:
  def delay3(b:B) = innerIdent(combiner(b,  3))
  def delay2(b:B) = delay3(combiner(b, 2))
  def delay1(b:B) = delay2(combiner(b,  1))
  delay1(Nil)

  Becomes:
  def delay2(b:B) = innerIdent(combiner(combiner(b, 2),  3))
  def delay1(b:B) = delay2(combiner(b,  1))
  delay1(Nil)

  Equivalent:
  def delay1(b:B) = innerIdent(combiner(combiner(combiner(b,  1), 2),  3))
  delay1(Nil)

  Equivalent:
  ((b:B) => innerIdent(combiner(combiner(combiner(b,  1), 2),  3)))(Nil)

  Equivalent:
  innerIdent(combiner(combiner(combiner(Nil,  1), 2),  3))
  */

}
