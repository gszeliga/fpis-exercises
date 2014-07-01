package cn.fpis.exercises.ch11

object Functors {
  val listFunctor = new Functor[List] {
    def map[A, B](a: List[A])(f: A => B): List[B] = a map f
  }
  

}