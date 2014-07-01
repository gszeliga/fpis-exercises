package cn.fpis.exercises.ch11

object TestFunctor {

  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = ???
  }

}