package cn.fpis.exercises.ch13

import cn.fpis.exercises.ch11.Monad
import cn.fpis.exercises.ch13.Player.winnerMsg

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
  def readLine2: IO[String] = new IO[String]{def run = scala.io.StdIn.readLine}

  def printWinner(p: Player) = printLine(winnerMsg(p))
  def empty: IO[Unit] = new IO[Unit] {def run = ()}

  override def unit[A](a: => A): IO[A] = new IO[A]{def run = a}
  override def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = ma flatMap f
  def apply[A](a: => A): IO[A] = unit(a)
}
