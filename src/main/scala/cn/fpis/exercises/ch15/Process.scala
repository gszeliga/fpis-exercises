package cn.fpis.exercises.ch15

import cn.fpis.exercises.ch11.Monad

/*
This is a Transducer: It transforms a Stream containing 'I' values into a Stream of 'O' values
* */
trait Process[I,O]{

  def apply(s: Stream[I]):Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv,finalizer) => s match {
      case h #:: t => recv(Some(h))(t) //We keep the interpretation loop by passing tail to 'apply'
      case _ => finalizer(s)
    }
    case Emit(h,t) => Stream(h) append t(s)
  }

  def map[O2](f: O => O2): Process[I,O2] = this match {
    case Halt() => Halt()
    case Await(recv, finalizer) => Await(recv andThen (_ map f), finalizer map f)
    case Emit(h,t) =>  Emit(f(h), t map f)
  }

  def ++(p: => Process[I,O]): Process[I,O] = this match {

    case Halt() => p
    case Await(recv, finalizer) => Await(i => recv(i) ++ p, finalizer ++ p)
    case Emit(h,t) => Emit(h,t ++ p)
  }

/*  def emitAll[I,O](head: Seq[O], tail: Process[I,O] = Halt[I,O]()): Process[I,O] = {
    tail match {
      case Emit(h,t) => Emit(head ++ h, t)
      case _ => Emit(head, tail)
    }
  }*/

  def emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]):Process[I,O] = Emit(head,tail)

  def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
    case Halt() => Halt()
    case Emit(h,t) => f(h) ++ t.flatMap(f)
    case Await(recv, finalizer) => Await(i => recv(i) flatMap f, finalizer flatMap f)
  }

  def unit[I,O](v:O): Process[I,O] = emit(v)

  //'pipe' or 'compose'
  def |>[O2](p2: Process[O,O2]): Process[I,O2] = {

    p2 match {
      case Halt() => Halt()
      case Emit(h,t) => Emit(h,this |> t)
      case Await(f,finalizer) => this match {
        case Emit(h,t) => t |> f(Some(h))
        case Halt() => Halt() |> f(None)
        case Await(g,finalizer2) =>  Await(i => g(i) |> p2)
      }
    }
  }

  def lift[I,O](f: I => O): Process[I,O] = {
    /*Await(i => i.map( v => Emit[I,O](f(v))).getOrElse(Halt()))*/

    Await({
      case Some(v) => Emit(f(v))
      case None => Halt()
    })
  }

  def repeat: Process[I,O] = {

    def go(p: Process[I,O]): Process[I,O] = {
      p match{
        case Halt() => go(this)
        case Emit(h,t) => Emit(h, go(t))
        case Await(r,f) => Await(r andThen go,f)
      }
    }

    go(this)
  }

  def filter[I,O](f: I => Boolean) = Await[I,I]({
    case Some(v) if f(v) => Emit(v)
    case None => Halt()
  }) repeat

  def take[I](n: Int): Process[I,I] = {
    if(n <= 0) Halt()
    else{
      Await[I,I](_.map(i => Emit(i, take[I](n-1))).getOrElse(Halt()))
    }
  }

  //Echoes back the incoming input
  def id[I]: Process[I,I] = lift(identity)

  def drop[I](n: Int): Process[I,I] = {
    if(n == 0) id //emits the input as it comes in (cool shit)
    else Await[I,I](_ => drop(n-1))
  }

  def takeWhile[I](f: I => Boolean): Process[I,I] = {
    Await[I,I](_.map(i => if(f(i)) Emit[I,I](i, takeWhile(f)) else Halt[I,I]()).getOrElse(Halt()))
  }

  def dropWhile[I](f: I => Boolean): Process[I,I] = {
    Await(_.map(i => if(f(i)) dropWhile(f) else Emit[I,I](i) ).getOrElse(Halt()))
  }

}

object Process{

  //Partially apply the I parameter of Process
  def monad[I] = new Monad[({ type f[x] = Process[I,x]})#f]{
    override def unit[O](v: => O): Process[I, O] = Emit(v)
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
case class Emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]) extends Process[I,O]

/*
* Requests a value from the input stream, indicating that 'recv'
should be used by the driver to produce the next state, and that finalizer should be
consulted if the input has no more elements available. (pull oriented approach)
* */
case class Await[I,O](recv: Option[I] => Process[I,O], finalizer: Process[I,O] = Halt[I,O]) extends Process[I,O]