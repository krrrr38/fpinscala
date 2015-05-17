package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps
import fpinscala.testing.{Prop, Gen}

// infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2
    def zero: (A) => A = a => a
  }

  // XXX
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)
    def zero: A = m.zero
  }

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val p1 = Prop.forAll { for {
      v1 <- gen
      v2 <- gen
      v3 <- gen
    } yield (v1, v2, v3) } { case (v1, v2, v3) =>
      m.op(m.op(v1, v2), v3) == m.op(v1, m.op(v2, v3))
    }
    val p2 = Prop.forAll(gen)(x => m.op(x, m.zero) == x && m.op(m.zero, x) == x)
    p1 && p2
  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))

  def foldMapWithFoldLeft[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    foldLeft(as)(m.zero)((acc, a) => m.op(acc, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(a, b))(z)

  // XXX using `dual`
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as.head)
    else {
      val (as1, as2) = as.splitAt(as.length / 2)
      m.op(foldMapV(as1, m)(f), foldMapV(as2, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = for {
      v1 <- a1
      v2 <- a2
    } yield m.op(v1, v2)
    def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.flatMap(Par.sequenceBalanced(v.map(Par.asyncF(f)))) { bs =>
      foldMapV(bs, par(m))(b => Par.unit(b))
    }

  lazy val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Part(l1, w1, r1), Stub(c2)) => Part(l1, w1, r1 + c2)
      case (Stub(c1), Part(l2, w2, r2)) => Part(c1 + l2, w2, r2)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
    }
    def zero: WC = Stub("")
  }

  def count(s: String): Int = foldMapV(s.toList.toIndexedSeq, wcMonoid) { char =>
    if (char.isWhitespace) Part("", 0, "") else Stub(char.toString)
  } match {
    case Stub(c) => if (c.isEmpty) 0 else 1
    case Part(l, w, r) => (if (l.isEmpty) 0 else 1) + w + (if (r.isEmpty) 0 else 1)
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    def zero: (A, B) = (A.zero, B.zero)
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))
    def zero: (A) => B = a => B.zero
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
      }
    def zero: Map[K, V] = Map.empty
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)((a: A) => (b: B) => f(a, b))(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)((a: A) => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldRight(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(value) => f(value)
    case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(value) => f(z, value)
    case Branch(left, right) => foldLeft(right)(foldLeft(right)(z)(f))(f)
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(value) => f(value, z)
    case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(a) => f(z, a)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}

object MonoidMain {
  import Gen._
  import Prop._
  import Monoid._

  val int = smallInt
  val optionInt = smallInt.map(Some.apply)

  def main(args: Array[String]) {
    run(monoidLaws(listMonoid[Int], listOfN(5, int)))
    run(monoidLaws(stringMonoid, int.flatMap(stringN)))
    run(monoidLaws(intAddition, int))
    run(monoidLaws(intMultiplication, int))
    run(monoidLaws(booleanOr, boolean))
    run(monoidLaws(booleanAnd, boolean))
    run(monoidLaws(optionMonoid[Int], optionInt))
    // To pass the test, we need to define Equal[Function1] and use it on the monoidLaw
    // run(monoidLaws(endoMonoid[Int], genEndoFn))
    run(monoidLaws(wcMonoid, genWC))

    println(count("lorem ipsum dolor sit amet, ") == 5)
    println(count("lorem  ") == 1)
    println(count("lorem          ") == 1)

    val res = bag(Vector("a", "rose", "is", "a", "rose"))
    println(res.get("a").contains(2))
    println(res.get("rose").contains(2))
    println(res.get("is").contains(1))
  }

  private def genEndoFn: Gen[Int => Int] = int.map(i => v => i*v)
  private def genWC: Gen[WC] = boolean.flatMap { bool =>
    if (bool)
      int.flatMap(i => stringN(i).map(Stub))
    else for {
      w <- int
      l1 <- int
      left <- stringN(l1)
      l2 <- int
      right <- stringN(l2)
    } yield Part(left, w, right)
  }
}