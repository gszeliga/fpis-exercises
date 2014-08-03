package cn.fpis.exercises.ch11

import cn.fpis.exercises.ch6.State

object Monads {
  val listMonad = new Monad[List] {

    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f

  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Option(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  // Since `State` is a binary type constructor, we need to partially apply it
  // with the `S` type argument. Thus, it is not just one monad, but an entire
  // family of monads, one for each type `S`. One solution is to create a class
  // `StateMonads` that accepts the `S` type argument and then has a _type member_
  // for the fully applied `State[S, A]` type inside:  
  class StateMonads[S] {
    type StateS[A] = State[S, A]

    val monad = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State.unit(a)
      def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma flatMap f
    }

  }

  //https://github.com/pchiusano/fpinscala/blob/master/answers/src/main/scala/fpinscala/monads/Monad.scala
  //This is what is known as a type lambda
  def stateMonad[S] = new Monad[({type lambda[x] = State[S,x]})#lambda]{
    
    def unit[A](a: => A): State[S,A] = State(s => (a, s))
    def flatMap[A, B](ma: State[S,A])(f: A => State[S,B]): State[S,B] = ma flatMap f
    
  }

  /*
 * Applying Id to A is an identity since the wrapped type and the unwrapped type are totally isomorphic 
 * (we can go from one to the other and back again without any loss of information)
 * */
  case class Id[A](value: A) {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
    def map[B](f: A => B): Id[B] = unit(f(value))
  }

}