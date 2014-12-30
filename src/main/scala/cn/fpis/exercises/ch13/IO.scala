package cn.fpis.exercises.ch13

import cn.fpis.exercises.ch11.Monad
import cn.fpis.exercises.ch13.Player.winnerMsg

import scala.annotation.tailrec

/**
 * Created by guillermo on 4/12/14.
 */

trait IO[+A] {

  self =>

  def run: A

  def **[B](io: IO[B]): IO[(A,B)] = new IO[(A,B)]{
    def run = {
      (self.run, io.run)
    }
  }

  def map[B](f: A => B): IO[B] = new IO[B]{def run = f(self.run)}
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B]{def run = f(self.run).run}
}

object IO extends Monad[IO]{

  def printLine(msg: String) = new IO[Unit]{def run = println(msg)}
  def readLine2: IO[String] = new IO[String]{def run = {
    println("Reading...")
    scala.io.StdIn.readLine}
  }

  def printWinner(p: Player) = printLine(winnerMsg(p))
  def empty: IO[Unit] = new IO[Unit] {def run = ()}

  override def unit[A](a: => A): IO[A] = new IO[A]{def run = a}
  override def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = ma flatMap f
  def apply[A](a: => A): IO[A] = unit(a)
  def ref[A](a: => A): IO[IORef[A]] = IO { new IORef(a) }

  sealed class IORef[A](var value: A) {
    def set(a: A): IO[A] = IO{value = a; a}
    def get: IO[A] = IO{value}
    def modify(f: A => A):IO[A] = get flatMap {a => set(f(a))}
  }

  def factorial(n: Int) = for{
    acc <- ref(1)
    _ <- foreachM(1 to n toStream)(i => skip(acc.modify(_ * i)))
  } yield(acc.get)


  val factorialREPL = sequence_(
    printLine("Enter any number"),

    doWhile(readLine2) { l =>

      val continue= l != 'q

      when(continue) {
        for {
          fact <- factorial(l.toInt)
          _ <- fact flatMap (v => printLine("Result is " + v))
        } yield ()
      }
    }
  )
}

trait ExtIO[F[_],+A]
case class Pure[F[_],+A](a: A) extends ExtIO[F,A]
//While F[I] is the expression (external side-effect) the receive is the continuation
case class Request[F[_],I,+A](expr: F[I], receive: I => ExtIO[F,A]) extends ExtIO[F,A]

trait Console[A]
case object ReadLine extends Console[Option[String]]
case class PrintLine(l: String) extends Console[Unit]

trait Run[F[_]]{
  def apply[A](expr: F[A]): (A, Run[F])
}

object ExtIO
{
  @tailrec
  def run[F[_],A](R: Run[F])(operation: ExtIO[F,A]): A = {
    operation match {
      case Pure(a) => a
      case Request(exp, receive) => {
        R(exp) match {
          case (result, interpreter) => run(interpreter)(receive(result))
        }
      }
    }
  }

  def monad[F[_]] = new Monad[({type f[a] = ExtIO[F,a]})#f]{
    override def unit[A](a: => A): ExtIO[F, A] = new Pure(a)
    override def flatMap[A, B](ma: ExtIO[F, A])(f: (A) => ExtIO[F, B]): ExtIO[F, B] = {
      ma match {
        case Pure(a) => f(a)
        case Request(expr, receive) => ???
      }
    }
  }

}

object RunConsole extends Run[Console]
{
  override def apply[A](expr: Console[A]): (A, Run[Console]) = expr match {
    case ReadLine => {
      try{
        (Some(scala.io.StdIn.readLine()), RunConsole)
      }
      catch
      {
        case _ : Throwable => (None, RunConsole)
      }
    }
    case PrintLine(s) => (println(s), RunConsole)
  }
}