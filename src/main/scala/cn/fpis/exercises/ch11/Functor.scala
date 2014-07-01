package cn.fpis.exercises.ch11

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  
  //This is a formal way of defining the 'unzip' method
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = {
    (map(fab)(_._1), map(fab)(_._2))
  }
}