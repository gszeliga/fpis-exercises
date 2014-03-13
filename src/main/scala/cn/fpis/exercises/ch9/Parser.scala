package cn.fpis.exercises.ch9

import scala.util.matching.Regex
import java.util.regex.Pattern

trait Parsers {

  self =>

  type Parser[+A] = Parsers.Parser[A]

  implicit def string(s: String): Parser[String] = {

    l =>
      {
        if (l.input.startsWith(s)) Success(s, s.length)
        else Failure(l.toError(s"Expected: $s"))
      }

  }

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def regex(r: Regex): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]) = ParserOps(f(a))
  implicit def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)
    def as[B](v: B): Parser[B] = self.map(p.slice)(_ => v)
    def label(l: String): Parser[A] = self.label(l)(p)
    def separatedBy(sep: Parser[String]): Parser[List[A]] = self.separatedBy(p, sep)
    def many = self.many(p)
    def skipL(s: Parser[String]) = self.skipL(s, p)
  }

  def whitespaces: Parser[String] = "\\s*".r
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def token[A](p: Parser[A]) = skipR(attemp(p), whitespaces)
  def double: Parser[Double] = doubleString map (_.toDouble) label ("doubles")

  //Reads characters until s found
  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  def quoted = skipL(string("\""), thru("\""))
  def unquotedString = token(quoted) label "string"

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def succeed[A](a: A): Parser[A] = l => string("").map(_ => a)(l)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]] = {
    or(map2(p, many(p))(_ :: _), succeed(Nil))
  }

  //We want to recognize one or more instances of A
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_ :: _)
  }

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = {
    flatMap(a)(v => succeed(f(v)))
  }

  //Slice consumes input until exhausted and returns the consumed sliced value
  def slice[A](p: Parser[A]): Parser[String] = {
    location =>
      {
        many(p).map(_.mkString)(location)
      }
  }

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = flatMap(p)(v1 => p2 map (v2 => (v1, v2)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = flatMap(p)(v1 => p2 map (v2 => f(v1, v2))) //map(product(p, p2))(f.tupled)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _)
    else succeed(Nil)
  }

  def separatedBy[A](a: Parser[A], sep: Parser[String]): Parser[List[A]] = {
    map2(a, many(skipL(sep, a)))(_ :: _) or succeed(Nil)
  }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = {
    l =>
      {
        p(l) match {
          case Success(r, c) => {
            val al = l.advance(c)
            f(r)(al) mapError (_.push(al, "Second flatMap parser failed"))
          }
          case f @ _ => f.mapError(_.push(l, "First flatMap parser failed")).asInstanceOf[Result[B]]
        }
      }
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] = {
    l => p(l) mapError (_.label(msg))
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = {
    l => p(l) mapError (_.push(l, msg))
  }

  def attemp[A](p: Parser[A]): Parser[A]

  def skipL[A, B](p: Parser[A], p2: Parser[B]) = map2(slice(p), p2)((_, b) => b)
  def skipR[A, B](p: Parser[A], p2: Parser[B]) = map2(p, slice(p2))((a, _) => a)
  def surrounded[A, B, C](pl: Parser[A], pr: Parser[B])(p: Parser[C]) = skipL(pl, skipR(p, pr))

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] =
    skipR(p, eof)

  def seq[U, A, B](f: U => Parser[A])(g: A => Parser[B]): U => Parser[B] = {
    u => f(u) flatMap (g)
  }

}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError = ParseError(List((this, msg)))
  def advance(n: Int) = copy(offset = offset + n)

}

case class ParseError(stack: List[(Location, String)] = Nil, otherFailures: List[ParseError] = Nil) {
  def push(loc: Location, msg: String) = copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError = {
    ParseError(latestLoc.map((_, s)).toList, otherFailures map (_.label(s)))
  }

  def latest: Option[(Location, String)] = stack.lastOption
  def latestLoc: Option[Location] = latest map (_._1)
  def furthest = copy(otherFailures = List()) :: otherFailures maxBy (_.latest.map(_._1.offset))

}

trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = {
    this match {
      case Failure(pe) => Failure(f(pe))
      case _ => this
    }
  }
}

case class Success[+A](get: A, consumed: Int) extends Result[A]
case class Failure(get: ParseError) extends Result[Nothing]

object Parsers {
  type Parser[+A] = Location => Result[A]
}
