package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case _ if n <= 0 => empty
    case Cons(h, t) => Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0=> t().drop(n - 1)
    case _ if n <= 0 => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => empty
  }

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = this.foldRight(empty[A]) { (a, stream) =>
    if (p(a)) cons(a, stream)
    else empty
  }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)(p(_) && _)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def hedOptionWithFoldRight: Option[A] = this.foldRight(None: Option[A])((a, z) => Some(a))

  def map[B](f: A => B): Stream[B] = this.foldRight(empty[B])((a, z) => cons(f(a), z))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(empty[A]) { (a, z) =>
      if (f(a)) cons(a, z)
      else z
    }

  def append[B>:A](that: Stream[B]): Stream[B] = this.foldRight(that)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B]) { (a, z) =>
    f(a) append z
  }

  def mapWithUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(f(h()), t())
  }

  def takeWithUnFold(n: Int): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if n > 0 => Some(h(), t().takeWithUnFold(n - 1))
    case _ => None
  }

  def takeWhileWithUnFold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t().takeWhileWithUnFold(p))
    case _ => None
  }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = (this, s2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(() => (Some(h1()), Some(h2())), () => t1().zipAll(t2()))
    case (Empty, Cons(h2, t2)) => Cons(() => (None, Some(h2())), () => empty.zipAll(t2()))
    case (Cons(h1, t1), Empty) => Cons(() => (Some(h1()), None), () => t1().zipAll(empty))
    case _ => empty
  }

  def zipAllWithUnFold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), empty) =>  Some((Some(h1()), None), (t1(), empty))
    case (empty, Cons(h2, t2)) =>  Some((None, Some(h2())), (empty, t2()))
    case _ => None
  }

  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s).takeWhile(v => v._1.isDefined && v._2.isDefined).forAll(v => v._1 == v._2)

  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case c@Cons(h, t) => Some(c, t())
    case Empty => None
  } append empty

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists(_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = this.foldRight((z, Stream(z))) { (a, b) =>
    val v = f(a, b._1)
    (v, cons(v, b._2))
  }._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = {
    lazy val _constant: Stream[A] = Cons(() => a, () => _constant)
    _constant
  }
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def _fibs(pre: Int, cur: Int): Stream[Int] =
      Stream.cons(pre + cur, _fibs(cur, pre + cur))
    _fibs(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Empty
  }

  def fibsWithUnFold: Stream[Int] = unfold((0, 1))(v => Some(v._1, (v._2, v._1 + v._2)))

  def fromWithUnFold(n: Int): Stream[Int] = unfold(n)(i => Some(i, i + 1))

  def constantWithUnFold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def onesWithUnFold: Stream[Int] = unfold(())(_ => Some(1, ()))
}