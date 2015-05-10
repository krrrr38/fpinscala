package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (max, n, rng) => run(max, n, rng) match {
    case Passed => p.run(max, n, rng)
    case v => v
  }}

  def ||(p: Prop): Prop = Prop { (max, n, rng) => run(max, n, rng) match {
    case Falsified(_, _) => p.run(max, n, rng)
    case _ => Passed
  }}
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = true
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  val ES: ExecutorService = Executors.newCachedThreadPool()
  val p1 = forAll(Gen.unit(Par.unit(1))) { i =>
    Par.map(i)(_ + 1)(ES).get() == Par.unit(2)(ES).get
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed
    else Falsified("()", 0)
  }

  val p2 = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p3 = check {
    equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2))(ES).get
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool()) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  val pint = Gen.choose(0, 10) map Par.unit
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(identity), n))

  val p5 = forAllPar(pint)(n => equal(Par.unit(n)(ES).get(), n))

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))
  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)
  def listOf1(size: Int): Gen[List[A]] = Gen.listOfN(size max 1, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)
  def listOf1(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOf1)
  def unsized: SGen[A] = SGen(int => this)
  def map2[B,C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen.map2(this, g)(f)
  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State((a, _)))

  def int: Gen[Int] = Gen(State(RNG.int))

  def double: Gen[Double] = Gen(State(RNG.double))

  def boolean: Gen[Boolean] = int.map(_%2 == 0)

  def string: SGen[String] = listOf(int).map(_.map(_.toChar).mkString)

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g map (i => s => i)

  def optionInt: Gen[Option[Int]] = int.toOption

  def fpoptionInt: Gen[fpinscala.errorhandling.Option[Int]] = int.toFpOption

  val smallInt = Gen.choose(-10, 10)

  val maxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val sortedProp = forAll(listOf(smallInt)) {
    case Nil => true
    case x::Nil => true
    case ns =>
      val sorted = ns.sorted
      sorted.zip(sorted.tail).forall { case (a, b) => a <= b }
  }

  val isEven = (i: Int) => i%2 == 0
  val takeWhileProp = forAll(Gen.listOf(int))(ns => ns.takeWhile(isEven).forall(isEven))

  val takeProp = forAll(listOf(int).flatMap(ns => pair(choose(0, ns.length), ns))) { case (i, ns) =>
    val taken = ns.take(i)
    taken.length == i && taken.zipWithIndex.forall { case (v, index) => v == ns(index) }
  }

  val sequenceFpOptionProp = forAll(listOf(fpoptionInt)) { ns =>
    val hasNone = ns.exists(_.isEmpty)
    val result = fpinscala.errorhandling.Option.sequence(ns)
    if (hasNone) result.isEmpty else result.isDefined
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(diff => start + diff))

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    pair(choose(start, stopExclusive), choose(start, stopExclusive))

  def map2[A,B,C](a: Gen[A], b: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(a.sample.map2(b.sample)(f))

  def pair[A,B](a: Gen[A], b: Gen[B]): Gen[(A,B)] = map2(a, b)((va, vb) => (va, vb))

  def pair[A,B](a: Gen[A], b: B): Gen[(A, B)] = Gen(a.sample.map(va => (va, b)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2 / (g1._2 + g2._2)
    double.flatMap(d => if (d < threshold) g1._1 else g2._1)
  }

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  def listOf[A](a: Gen[A]): SGen[List[A]] = SGen(size => listOfN(size, a))

  def listOf1[A](a: Gen[A]): SGen[List[A]] = SGen(size => listOfN(size max 1, a))

  def run(p: Prop,
           maxSize: Int = 100,
           testCases: Int = 100,
           rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }

  implicit class OptionGen[A](gen: Gen[A]) {
    def toOption: Gen[Option[A]] =
      Gen(gen.sample.map2(boolean.sample)((v, bool) => if (bool) Some(v) else None))

    def toFpOption: Gen[fpinscala.errorhandling.Option[A]] =
      Gen(gen.sample.map2(boolean.sample)((v, bool) => if (bool) fpinscala.errorhandling.Some(v) else fpinscala.errorhandling.None))
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen(int => forSize(int).map(f))
  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(int => forSize(int).flatMap(f(_)))
}

