package cn.fpis.exercises.ch12

import cn.fpis.exercises.ch11.Functor
import scala.annotation.tailrec
import cn.fpis.exercises.ch11.Monad

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
}
