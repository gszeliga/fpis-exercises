package cn.fpis.exercises.ch10

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as)(f.curried)(endoMonoid[B])(z)
  }

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)
  }

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  }

  def concatenate[A](as: F[A])(m: Monoid[A]): A = {
    foldLeft(as)(m.zero)(m.op)
  }

  def toList[A](as: F[A]): List[A] = {
    foldRight(as)(List.empty[A])(_ :: _)
  }
}
