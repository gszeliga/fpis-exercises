package cn.fpis.exercises.ch11

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = {
    flatMap(ma)(a => unit(f(a)))
  }

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = {
    flatMap(ma)(a => map(mb)(b => f(a, b)))
  }

}