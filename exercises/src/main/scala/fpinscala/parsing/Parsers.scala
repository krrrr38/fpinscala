package fpinscala.parsing

import java.util.regex._
import fpinscala.parsing.MyParserDef.MyParser

import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  val daParser = "\\d".r.flatMap(n => listOfN(n.toInt, char('a')))

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.head)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(Nil)
    else map2(p, listOfN(n - 1, p))(_ :: _)

  // s1で実行してみて、成功ならコミット、途中で失敗ならコミットされていない状態から同じ入力でs2を実行
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(Nil)

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  // def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  def productWithMap2[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = map2(p, p2)((a, b) => (a, b))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a, b)))

  def map2WithProduct[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    product(p, p2) map f.tupled

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  // 解析へのコミットを先送りさせるもの
  def attempt[A](p: Parser[A]): Parser[A]

  def sep[A,B](p: Parser[A], separator: Parser[B]): Parser[List[A]] =
    sep1(p, separator) or succeed(Nil)

  def sep1[A,B](p: Parser[A], separator: Parser[B]): Parser[List[A]] =
    map2(p, many(separator ~> p))(_ :: _)

  def ignoreLeft[A,B](ignore: Parser[A], p: => Parser[B]): Parser[B] =
    map2(slice(ignore), p)((_, b) => b)

  def ignoreRight[A,B](p: Parser[A], ignore: => Parser[B]): Parser[A] =
    map2(p, slice(ignore))((a, _) => a)

  def number = regex("\\d+(\\.\\d+)?".r)

  def space = regex("\\s*".r)

  def trim[A](p: Parser[A]) = p <~ space

  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def <~[B](ignore: => Parser[B]): Parser[A] = self.ignoreRight(p, ignore)
    def ~>[B](parser: => Parser[B]): Parser[B] = self.ignoreLeft(p, parser)
    def rep[B](separator: => Parser[B]): Parser[List[A]] = self.sep(p, separator)
    def rep1[B](separator: => Parser[B]): Parser[List[A]] = self.sep1(p, separator)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_,s)).toList)

  def latestLoc: Option[Location] = latest map (_._1)

  def latest: Option[(Location, String)] = stack.lastOption

  def show: Unit = stack.foreach { case (loc, msg) =>
    val dropped = loc.input.drop(loc.offset - 1)
    println(s"- $msg")
    println(s"Cannot parse: [${dropped.head}]${dropped.tail.take(10)}")
  }
}

object MyParserDef {
  type MyParser[+A] = Location => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, b) => Failure(f(e), b)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n+m)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
}

object MyParsers extends Parsers[MyParser] {
  import MyParserDef._

  override def run[A](p: MyParser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Success(get, _) => Right(get)
      case Failure(get, _) => Left(get)
    }

  override def flatMap[A, B](p: MyParser[A])(f: (A) => MyParser[B]): MyParser[B] = (loc: Location) =>
    p(loc) match {
      case Success(get, charsConsumed) =>
        f(get)(loc.advanceBy(charsConsumed))
          .addCommit(charsConsumed != 0)
          .advanceSuccess(charsConsumed)
      case f:Failure => f
    }

  // s1で実行してみて、成功ならコミット、途中で失敗ならコミットされていない状態から同じ入力でs2を実行
  override def or[A](s1: MyParser[A], s2: => MyParser[A]): MyParser[A] = (loc: Location) =>
    s1(loc) match {
      case Failure(e, false) => s2(loc)
      case r => r
    }

  override implicit def string(s: String): MyParser[String] = (loc: Location) =>
    if (loc.input.startsWith(s))
      Success(s, s.length)
    else
      Failure(loc.toError(s"Expected: $s"), false)

  override def slice[A](p: MyParser[A]): MyParser[String] = (loc: Location) =>
    p(loc) match {
      case Success(_, charsConsumed) => Success(loc.input.substring(loc.offset, charsConsumed), charsConsumed)
      case f:Failure => f
    }

  override def scope[A](msg: String)(p: MyParser[A]): MyParser[A] = (loc: Location) =>
    p(loc).mapError(_.push(loc, msg))


  override implicit def regex(r: Regex): MyParser[String] = (loc: Location) =>
    r.findFirstMatchIn(loc.input) match {
      case Some(m) =>
        val matched = m.matched
        Success(matched, matched.length)
      case None => Failure(loc.toError(s"Expected: regex ${r.toString()}"), false)
    }

  override def label[A](msg: String)(p: MyParser[A]): MyParser[A] = (loc: Location) =>
    p(loc).mapError(_.label(msg))

  // 解析へのコミットを先送りさせるもの
  override def attempt[A](p: MyParser[A]): MyParser[A] = (loc: Location) =>
    p(loc).uncommit

  override def succeed[A](a: A): MyParser[A] = (loc: Location) => Success(a, loc.offset)
}