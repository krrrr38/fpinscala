package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail should be called from NEL.")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead should be called from NEL.")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumWithFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productWithFoldLeft(ints: List[Int]): Int = foldLeft(ints, 1)(_ * _)

  def lengthWithFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)((_, acc) => acc + 1)

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append(reverse(t), List(h))
  }

  def reverseWithFoldRight[A](l: List[A]): List[A] =
    foldRight(l, Nil: List[A])((a, l) => Cons(a, l))

  def foldLeftWithFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def appendWithFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a2, a1)((ls, a) => append(ls, List(a)))

  def appendWithFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, ls) => Cons(a, ls))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((es, ls) => append(es, ls))

  def incrementMap(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((n, ls) => Cons(n + 1, ls))

  def doubleToStringMap(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((d, ls) => Cons(d.toString, ls))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, ls) => Cons(f(a), ls))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil: List[A])((a, ls) => if (f(a)) Cons(a, ls) else ls)

  def odds(l: List[Int]): List[Int] = filter(l)(_ % 2 != 0)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  def zipPlus(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(la, lt), Cons(ra, rt)) => Cons(la + ra, zipPlus(lt, rt))
  }

  def zipWith[A, B](l: List[A], r: List[A])(f: (A, A) => B): List[B] = (l, r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(la, lt), Cons(ra, rt)) => Cons(f(la, ra), zipWith(lt, rt)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasPrefix(ls: List[A], prefix: List[A]): Boolean = (ls, prefix) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(a, as), Cons(p, ps)) =>
        if (a == p) hasPrefix(as, ps) else false
    }

    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(a, ta), Cons(b, tb)) =>
        if (hasPrefix(sup, sub)) true
        else hasSubsequence(ta, sub)
    }
  }
}

object ListMain {

  import List._

  def main(args: Array[String]) {
    println("--- All hasSubsequence results should be true. ---")
    println(hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2)))
    println(hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4)))
    println(hasSubsequence(List(4, 5), List(5)))
    println(!hasSubsequence(List(1, 2, 3, 4, 5), List(0)))
    println(!hasSubsequence(List(1, 2, 3, 4, 5), List(2, 4)))
    println(!hasSubsequence(Nil, List(0)))
  }
}
