package cn.fpis.exercises.ch11

//import cn.fpis.exercises.ch11.Monads

object test {
  case class Id[A](value: A) {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
    def map[B](f: A => B): Id[B] = unit(f(value))
  }

  for {
    a <- Id("Hello, ")
    b <- Id("Monad!")
  } yield a + b                                   //> res0: cn.fpis.exercises.ch11.test.Id[String] = Id(Hello, Monad!)

  val M = Monads.stateMonad[Int]                  //> M  : cn.fpis.exercises.ch11.Monad[[x]cn.fpis.exercises.ch6.State[Int,x]] = c
                                                  //| n.fpis.exercises.ch11.Monads$$anon$4@71be98f5

  type IntTuple[+A] = (Int, A)

  val f = new Functor[IntTuple] {
    def map[A, B](fa: IntTuple[A])(f: A => B): IntTuple[B] = (fa._1, f(fa._2))
  }                                               //> f  : cn.fpis.exercises.ch11.Functor[cn.fpis.exercises.ch11.test.IntTuple] = 
                                                  //| cn.fpis.exercises.ch11.test$$anonfun$main$1$$anon$1@6fadae5d

	f.map((1,2))(a => a + 1)                  //> res1: cn.fpis.exercises.ch11.test.IntTuple[Int] = (1,3)

}