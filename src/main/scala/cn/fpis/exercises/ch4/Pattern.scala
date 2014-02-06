package ch.fpis.exercises.ch4

import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException

object PatternExample {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {

    a flatMap (va => b map (vb => f(va, vb)))

  }

  def pattern(s: String): Option[Pattern] = {

    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  }

  def mkMatcher(s: String): Option[String => Boolean] = {
    pattern(s) map (p => (v: String) => p.matcher(v).matches)
  }

  def bothMatch(p1: String, p2: String, s: String): Option[Boolean] = {

    map2(mkMatcher(p1), mkMatcher(p2))((a, b) => a(s) && b(s))

  }

  def sequence[A](lst: List[Option[A]]): Option[List[A]] = {

    lst.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a, b)(_ :: _))

  }

  def traverse[A, B](lst: List[A])(f: A => Option[B]): Option[List[B]] = {

    lst match {
      case Nil => Some(Nil)
      case h :: t => f(h) flatMap (vh => traverse(t)(f) map (vt => vh :: vt))
    }
  }

}