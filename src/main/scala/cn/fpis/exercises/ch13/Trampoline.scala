package cn.fpis.exercises.ch13

import java.nio.ByteBuffer
import java.nio.channels.{CompletionHandler, AsynchronousFileChannel}
import java.util.concurrent.{Callable, ExecutorService, CountDownLatch}

import scala.annotation.tailrec

/**
 * Created by guillermo on 29/12/14.
 */
trait Trampoline[+A]{
  def run: A = Trampoline.run(this)
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = {
    Trampoline.bind(this)(f)
  }
  def map[B](f: A => B): Trampoline[B] = {
    flatMap(f andThen Trampoline.done)
  }
}

case class State[A,S](run: S => Trampoline[(A,S)]){
  def map[B](f: A => B): State[B,S] = new State(
    s => {
      run(s) map {case (a,s) => (f(a),s)}
    }
  )

  def flatMap[B](f: A => State[B,S]): State[B,S] = new State(
    s => {
      Trampoline.more(() => run(s) flatMap {case (a,s) => Trampoline.more(() => f(a).run(s))})
    }
  )
}

//https://skillsmatter.com/skillscasts/3244-stackless-scala-free-monads
object Trampoline{

  case class Done[+A](get: A) extends Trampoline[A]
  case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]
  case class Bind[A, +B](force: () => Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

  def done[A](a:A): Done[A] = Done(a)
  def more[A](a: () => Trampoline[A]): More[A] = More(a)
  def delay[A](a: => A) = more(() => done(a))

  @tailrec
  //Like peeling an onion, we run all trampolines
  def run[A](t: Trampoline[A]): A = {
    t match {
      case Done(a) => a
      case More(f) => run(f())
      case Bind(f, g) => run(f() flatMap g)
    }
  }

  def bind[A,B](t: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] = {
    t match
    {
      case Done(get) => More(() => f(get))
      case More(force) => bind(force())(f)
      case Bind(force, g) => More(() => bind(force())(g andThen (_ flatMap f)))
    }
  }

  def odd[A](l: List[A]): Trampoline[Boolean] = {
    l match
    {
      case Nil => Done(true)
      case h :: t => More(() => even(t))
    }
  }

  def even[A](l: List[A]): Trampoline[Boolean] = {
    l match {
      case Nil => Done(false)
      case h :: t => More(() => odd(t))
    }

  }

}

//http://xuwei-k.github.io/scalaz-sxr/scalaz-2.9.2-7.0.0/concurrent/concurrent/Future.scala.html
object NonBlocking{

  def apply[A](a: => A) = More(() => Now(a))
  def apply[A](a: => A)(implicit service : ExecutorService): Future[A] = {
    Later(f => service submit(new Callable[Unit] {
      def call() = f(a).run
    }))
  }

  def read(file: AsynchronousFileChannel,
           fromPosition: Long,
           numBytes: Int): Later[Either[Throwable, Array[Byte]]] = {
    Later(f => {

      val buffer = ByteBuffer.allocate(numBytes)

      file.read(buffer, fromPosition, (), new CompletionHandler[Integer, Unit] {
        override def completed(readBytes: Integer, attachment: Unit): Unit = {
          val result = new Array[Byte](readBytes)
          buffer.slice().get(result, 0, readBytes)
          f(Right(result))
        }

        override def failed(exc: Throwable, attachment: Unit): Unit = f(Left(exc))
      })
    })
  }

  trait Future[+A] {

    def flatMap[B](f: A => Future[B]): Future[B] = this match {
      case Now(a) => More(() => f(a))
      case Later(listen) => BindLater(listen, f)
      case More(force) => BindMore(force, f)
      case BindLater(listen, g) => BindLater(listen, g andThen (_ flatMap f)) //Accumulates listeners
      case BindMore(force, g) => BindMore(force, g andThen (_ flatMap f)) //Accumulates listeners
    }

    def map[B](f: A => B): Future[B] = flatMap(a => More(() => Now(f(a))))

    //Forces the evaluation of futures into a result.
    @tailrec
    final def step: Future[A] = this match {
      case More(force) => force().step
      case BindMore(force, g) => (force() flatMap g).step
      case _ => this
    }

    //Runs computation up to obtaining an A
    def listen(f: A => Trampoline[Unit]): Unit = {
      this.step match {
        case Now(a) => f(a)
        case Later(listen) => listen(f)
        case BindLater(listen, g) => {
          listen(x => Trampoline.delay(g(x)) map (_ listen f))
        }
      }
    }

    def runAsync(f: A => Unit) = listen (a => Trampoline.done(f(a)))

    def run: A = this match{
      case Now(a) => a
      case _ => {

        val c = new CountDownLatch(1);
        @volatile var result = Option.empty[A]

        runAsync(a => {result = Some(a); c.countDown();})

        c.await();
        result.get
      }
    }

  }

  case class Now[+A](get: A) extends Future[A]
  case class Later[+A](listen: (A => Trampoline[Unit]) => Unit) extends Future[A]
  case class More[A](force: () => Future[A]) extends Future[A]
  case class BindLater[A, +B](listen: (A => Trampoline[Unit]) => Unit, f: A => Future[B]) extends Future[B]
  case class BindMore[  A, +B](force: () => Future[A], f: A => Future[B]) extends Future[B]


}