package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(get) => Some(f(get))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(get) => f(get)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def apply[A](a: A): Option[A] = if (a != null) Some(a) else None

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) map { _mean =>
      ???
    }
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for { va <- a; vb <- b } yield f(va, vb)

//  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
//    a.flatMap(va => b.map(vb => f(va, vb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head::tail => head.flatMap(v => sequence(tail) map (v :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a.map(f))
}

object OptionMain {
  import Option._

  def main(args: Array[String]) {
    println("--- All sequences return true.")
    println(sequence(List(Some(1))) == Some(List(1)))
    println(sequence(List(Some(1), Some(2))) == Some(List(1, 2)))
    println(sequence(List(Some(1), None, Some(2))) == None)
  }
}