package cn.fpis.exercises.ch12

import cn.fpis.exercises.ch11.Functor
import scala.annotation.tailrec
import cn.fpis.exercises.ch11.Monad
import java.util.Date

//A minimal implementation of Applicative must provide apply or map2.
trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fab, fa)((a, b) => a(b))
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    map2(unit(f), fa)(_(_))
  }

  def unit[A](a: A): F[A]
}

//All monads are applicative functors
trait ApplicativeMonad[M[_]] extends Applicative[M] {

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    join(map(ma)(a => f(a)))
  }

  def join[A](m: M[M[A]]): M[A] = flatMap(m)(v => v)

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = {
    a: A => flatMap(f(a))(vb => g(vb))
  }

  override def apply[A, B](fab: M[A => B])(fa: M[A]): M[B] = {
    flatMap(fab)(f => flatMap(fa)(a => unit(f(a))))
  }

  def sequence[A](fas: List[M[A]]): M[List[A]] = {
    fas.foldRight(unit(List.empty[A])) { (a, b) => map2(a, b)(_ :: _) }
  }

  def traverse[A, B](as: List[A])(f: A => M[B]): M[List[B]] = {
    as.foldRight(unit(List.empty[B])) { (a, b) => map2(f(a), b)(_ :: _) }
  }

  def replicateM[A](n: Int, fa: M[A]): M[List[A]] = {

    @tailrec
    def doReplicate(p: Int, fl: M[List[A]]): M[List[A]] = {
      if (p == 0) fl
      else {
        doReplicate(p - 1, map2(fa, fl)(_ :: _))
      }
    }

    doReplicate(n, unit(List.empty[A]))

  }

  def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] = {
    map2(ma, mb)((_, _))
  }

}

object Applicatives {
  def eitherMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
    def unit[A](a: => A): Either[E, A] = ???
    def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ???
  }

  //Applicative product??
  def product[G[_], F[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = ???

}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]
case class Success[A](s: A) extends Validation[Nothing, A]
case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object Validations {

  private def validationApplicative[E] = new Applicative[({ type f[x] = Validation[E, x] })#f] {
    def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Success(v1), Success(v2)) => Success(f(v1, v2))
        case (Failure(e1, t1), Failure(e2, t2)) => Failure(e1, t1 ++ (e2 +: t2))
        case (_, v @ Failure(h, t)) => v
        case (v @ Failure(h, t), _) => v
      }
    }
    def unit[A](a: A): Validation[E, A] = new Success(a)
  }

  def validName(name: String): Validation[String, String] = {
    if (name != "") Success(name)
    else Failure("Name cannot be empty", Vector.empty)
  }

  def validateBirthdate(birthdate: String): Validation[String, Date] = {
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case e: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd", Vector.empty)
    }
  }

  def validatePhone(phone: String): Validation[String, String] = {
    if (phone.matches("[0-9]{10}"))
      Success(phone)
    else Failure("Phone number must be 10 digits", Vector.empty)
  }

  def validate(name: String, birthdate: String, phone: String): Validation[String, WebForm] = {

    val applicative = validationApplicative[String]

    applicative.apply(applicative.apply(applicative.apply(applicative.unit((WebForm(_, _, _)).curried))(validName(name)))(validateBirthdate(birthdate)))(validatePhone(phone))
  }

}

