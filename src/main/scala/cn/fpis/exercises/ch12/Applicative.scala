package cn.fpis.exercises.ch12

import cn.fpis.exercises.ch11.Functor

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