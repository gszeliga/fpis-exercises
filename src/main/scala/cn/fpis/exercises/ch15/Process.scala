package cn.fpis.exercises.ch15

import cn.fpis.exercises.ch11.Monad

/*
This is a Transducer: It transforms a Stream containing 'I' values into a Stream of 'O' values
* */
trait Process[I,O]{

  def apply(s: Stream[I]):Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv,finalizer) => s match {
      case h #:: t => recv(h)(t) //We keep the interpretation loop by passing tail to 'apply'
      case _ => finalizer(s)
    }
    case Emit(h,t) => h.toStream append t(s)
  }

  def map[O2](f: O => O2): Process[I,O2] = this match {
    case Halt() => Halt()
    case Await(recv, finalizer) => Await(recv andThen (_ map f), finalizer map f)
    case Emit(h,t) =>  Emit(h map f, t map f)
  }

  def ++(p: => Process[I,O]): Process[I,O] = this match {

    case Halt() => p
    case Await(recv, finalizer) => Await(i => recv(i) ++ p, finalizer ++ p)
    case Emit(h,t) => emitAll(h,t ++ p)
  }

  def emitAll[I,O](head: Seq[O], tail: Process[I,O] = Halt[I,O]()): Process[I,O] = {
    tail match {
      case Emit(h,t) => Emit(head ++ h, t)
      case _ => Emit(head, tail)
    }
  }

  def emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]):Process[I,O] = emitAll(Stream(head),tail)

  def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
    case Halt() => Halt()
    case Emit(h,t) => {
      if (h.isEmpty) t flatMap f
      else f(h.head) ++ emitAll(h.tail,t).flatMap(f)
    }
    case Await(recv, finalizer) => Await(i => recv(i) flatMap f, finalizer flatMap f)
  }

  def unit[I,O](v:O): Process[I,O] = emit(v)

}

object Process{

  //Partially apply the I parameter of Process
  def monad[I] = new Monad[({ type f[x] = Process[I,x]})#f]{
    override def unit[O](v: => O): Process[I, O] = Emit(Stream(v))
    override def flatMap[O, O2](ma: Process[I, O])(f: (O) => Process[I, O2]): Process[I, O2] = ma flatMap f
  }

  implicit def toMonadic[I,O](p: Process[I,O]) = monad[I].toMonadic(p)

}

/*
Indicates to the driver that no more elements should be read from the input stream
or emitted to the output
* */
case class Halt[I,O]() extends Process[I,O]

/*Indicates to the driver that the head values should be emitted to the output stream,
 and that tail should be the next state following that*/
case class Emit[I,O](head: Seq[O], tail: Process[I,O] = Halt[I,O]) extends Process[I,O]

/*
* Requests a value from the input stream, indicating that 'recv'
should be used by the driver to produce the next state, and that finalizer should be
consulted if the input has no more elements available.
* */
case class Await[I,O](recv: I => Process[I,O], finalizer: Process[I,O] = Halt[I,O]) extends Process[I,O]