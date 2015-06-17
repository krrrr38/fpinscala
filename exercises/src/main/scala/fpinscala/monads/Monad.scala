package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = lma match {
    case Nil => unit(List.empty[A])
    case m::ms => map2(m, sequence(ms))(_ :: _)
  }

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    sequence(la map f)

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A,B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List.empty[A])) { (a: A, ml: M[List[A]]) =>
      compose(f, (b: Boolean) => if (b) map2(unit(a), ml)(_ :: _) else ml)(a)
    }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Implement in terms of `join` and `map`
  def _compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => join(map(f(a))(g))

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))

}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)
    def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = p.flatMap(ma)(f)
    def unit[A](a: => A): P[A] = p.succeed(a)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f
    def unit[A](a: => A): Option[A] = Option(a)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f
    def unit[A](a: => A): Stream[A] = Stream(a)
  }

  val listMonad: Monad[List] = new Monad[List] {
    def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
    def unit[A](a: => A): List[A] = List(a)
  }

  class StateMonal[S]  {
    type IntState[A] = State[Int, A]

    object IntStateMonad extends Monad[IntState] {
      def unit[A](a: => A): IntState[A] = State(s => (a, s))
      def flatMap[A, B](ma: IntState[A])(f: (A) => IntState[B]): IntState[B] = ma.flatMap(f)
    }

    object IntStateMonad2 extends Monad[({type λ[A] = State[Int, A]})#λ] {
      def unit[A](a: => A): State[Int, A] = ???
      def flatMap[A, B](ma: State[Int, A])(f: (A) => State[Int, B]): State[Int, B] = ???
    }
  }

  def stateMonad[S] = new Monad[({type λ[A] = State[S, A]})#λ] {
    def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma.flatMap(f)
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)
    def unit[A](a: => A): Id[A] = Id(a)
  }

  def readerMonad[R] = new Monad[({type λ[A] = Reader[R, A]})#λ] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](ma: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R, B] = Reader(r => f(ma.run(r)).run(r))
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = Reader(r => f(st.run(r)).run(r))
  }
}

object MonadMain {
  import Monad._
  val F = stateMonad[Int]
  def getState[S]: State[S,S] = State(s => (s,s))
  def setState[S](s: S): State[S,Unit] = State(_ => ((),s))

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
      xs <- acc
      n <- getState
      _ <- setState(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse

  val G = readerMonad[Int]
  val H = readerMonad[String]
  def twice = Reader((i: Int) => i*2)
  def noise = Reader((s: Int) => s + "!!!")
  def action(i: Int) = G.flatMap(twice)(a => noise).run(i)

  def main(args: Array[String]) {
    println(zipWithIndex(List("a", "b", "c", "d")))
    println(action(10))
  }
}
