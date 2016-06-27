package cn.fpis.exercises.ch15

import cn.fpis.exercises.ch11.Monad
import cn.fpis.exercises.ch15.Process.lift

import scala.language.postfixOps

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
    case Emit(h,t) => h #:: t(s)
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

  def repeat: Process[I,O] = {

    def go(p: Process[I,O]): Process[I,O] = {
      p match{
        case Halt() => go(this)
        case Emit(h,t) => Emit(h, go(t))
        case Await(r,f) => Await({
          case None => r(None)
          case s@Some(v) => go(r(s))
        },f)
      }
    }

    go(this)
  }

  def take[I](n: Int): Process[I,I] = {

    if(n == 0) Halt()
    else {
      Await[I,I]({
        case None => Halt()
        case Some(v) => Emit(v, take(n-1))
      })
    }

  }

  def drop[I](n: Int): Process[I,I] = {
    if(n == 0) id  //emits the input as it comes in (cool shit)
    else {
      Await[I, I]({
        case None => Halt()
        case Some(v) => drop(n - 1)
      })
    }
  }

  def takeWhile[I](f: I => Boolean): Process[I,I] = {
    Await[I,I]({
      case Some(v) if f(v) => Emit(v, takeWhile(f))
      case _ => Halt()
    })
  }

  def dropWhile[I](f: I => Boolean): Process[I,I] = {
    Await[I,I]({
      case Some(v) if f(v) => dropWhile(f)
      case Some(v) => Emit(v, id)
      case None => Halt()
    })
  }

  def count[I]: Process[I, Int] = {
    def go(c: Int): Process[I, Int] = {
      Await[I, Int](_ => Emit(c+1,go(c+1)))
    }

    go(0)
  }

  def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] = {
    Await[I,O]({
      case None => Halt()
      case Some(v) => f(v,z) match {
        case ((o,s)) => Emit(o, loop(s)(f))
      }
    })
  }

  def countWithLoop[I]: Process[I, Int] = loop(0)({
    case ((_,s)) => (s+1,s+1)
  })

  def filter[I,O](f: I => Boolean) = Await[I,I]({
    case Some(v) if f(v) => Emit(v)
    case None => Halt()
  }) repeat

  //Echoes back the incoming input
  def id[I]: Process[I,I] = lift(identity)

  def sum: Process[Double,Double] = {
    def go(s: Double): Process[Double, Double] = {
      //We emit the partial result and define as tail the next state to be accumulated
      Await{
        case Some(v) => Emit(v+s,go(v+s))
        case None => Halt()
      }
    }

    go(0.0)
  }

/*  def loop[S,I,O](z: S)(f: (S,I) => (S,O)): Process[I,O] = {
    Await(_.map(i => f(z,i) match {
      case (s,o) => emit(o,loop(s)(f))
    }).getOrElse(Halt()))
  }*/

  def sum2: Process[Double,Double] = loop(0.0)((acc,v) => (acc+v,acc+v))
  def count2[I]: Process[I,Int] = loop(0)((acc,_) => (acc+1,acc+1))

  //Zip feeds the same input to two different processes
  def zip[A,B,C](p1: Process[A,B])(p2: Process[A,C]): Process[A, (B,C)] = {
    (p1, p2) match {
      case (Halt(), _) => Halt()
      case (_, Halt()) => Halt()
      case (Emit(h1,t1), Emit(h2,t2)) => Emit((h1,h2), zip(t1)(t2))
      case (Await(recv1,f),_) => Await(v => zip(recv1(v))(feed(v)(p2)))
      case (_, Await(recv2,f)) => Await(v => zip(feed(v)(p1))(recv2(v)))
    }
  }

  def feed[A,B](v: Option[A])(p: Process[A,B]): Process[A,B] = p match{
    case Halt() => Halt()
    case Emit(h,t) => emit(h, feed(v)(t))
    /*case Await(r,f) => Await(k => feed(k)(r(v))) Does it get into an infinite loop?*/
    case Await(r,f) => r(v)
  }

  def zipWithIndex: Process[I,(O,Int)] = zip(this)(count)

  def exists[I](f: I => Boolean): Process[I, Boolean] = {
    //lift(f) will emit result result of applying the criteria
    //Whenever the result is 'true' it will turn the outcome of the loop as true
    lift(f) |> loop(false)((s,v) => (s || v, s || v))
  }

  //Does it actually work?
  def exists2[I](f: I => Boolean): Process[I,Boolean] = {
    takeWhile[I](i => !f(i)) match {
      case Halt() => Emit(false)
      case _ => Emit(true)
    }
  }
}

object Process{

  //Partially apply the I parameter of Process
  def monad[I] = new Monad[({ type f[x] = Process[I,x]})#f]{
    override def unit[O](v: => O): Process[I, O] = Emit(v)
    override def flatMap[O, O2](ma: Process[I, O])(f: (O) => Process[I, O2]): Process[I, O2] = ma flatMap f
  }

  def liftOne[I,O](f: I => O): Process[I,O] = {
    Await({
      case Some(v) => Emit(f(v))
      case None => Halt()
    })
  }

  def lift[I,O](f: I => O): Process[I,O] = liftOne(f).repeat

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