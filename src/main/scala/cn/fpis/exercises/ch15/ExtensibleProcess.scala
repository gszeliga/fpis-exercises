package cn.fpis.exercises.ch15

/**
  * Created by guillermo on 6/07/16.
  */
trait EProcess[F[_], O] {

  import EProcess._

  /**
    * Helper function to safely produce `p`, or gracefully halt
    * with an error if an exception is thrown.
    */
  def Try[F[_], O](p: => EProcess[F, O]): EProcess[F, O] =
    try p
    catch {
      case e: Throwable => Halt(e)
    }

  def onHalt(f: Throwable => EProcess[F, O]): EProcess[F, O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: => EProcess[F, O]): EProcess[F, O] = onHalt {
    case End => Try(p)
    case error => Halt(error)
  }

  def map[O2](f: O => O2): EProcess[F,O2] = this match {
    case Halt(e) => Halt(e)
    case Emit(h,t) => Emit(f(h), t map f)
    case Await(req,recv) =>  Await(req, recv andThen (_ map f ))
  }

  //Curried version of the Await constructor for better type inference
  def await[F[_],A,O](req: F[A], recv: Either[Throwable,A] => EProcess[F,O]): EProcess[F,O] = Await[F,A,O](req,recv)

  def flatMap[O2](f: O => EProcess[F,O2]): EProcess[F,O2] = this match {
    case Halt(e) => Halt(e)
    case Emit(h,t) => Try(f(h)) ++ t.flatMap(f)
    case Await(req,recv) => Await(req, recv andThen (_ flatMap f))
  }

}

object EProcess {

  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => EProcess[F, O]) extends EProcess[F, O]

  case class Emit[F[_], O](head: O, tail: EProcess[F, O]) extends EProcess[F, O]

  case class Halt[F[_], O](error: Throwable) extends EProcess[F, O]

  //Represent the case when there's no input
  case object End extends Exception

  //Process is being forcibly terminated
  case object Kill extends Exception

}