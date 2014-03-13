package cn.fpis.exercises.ch8

import cn.fpis.exercises.ch6.State
import cn.fpis.exercises.ch6.RNG
import ch.fpis.exercises.ch5.Stream

trait Status
case object Proven extends Status
case object Unfalsified extends Status

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  type Result = Either[FailedCase, (Status, SuccessCount)]
}

case class Prop(run: (Prop.TestCases, RNG) => Prop.Result) {

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, g) =>
      {

        def go(i: Int, j: Int, s: Stream[A], onEnd: Int => Prop.Result): Prop.Result = {
          if (i == j) Right((Unfalsified, i))
          else s.uncons match {
            case Some((h, t)) => {
              try {
                if (f(h)) go(i + 1, j, t, onEnd)
                else Left(h.toString)
              } catch {
                case e: Exception => Left(buildMsg(h, e))
              }
            }
            case None => onEnd(i)
          }
        }

        go(0, n / 3, a.exhaustive, i => Right(Proven, i)) match {
          case Right((Unfalsified, _)) =>
            val rands = randomStream(a)(g)
            go(n / 3, n, rands, i => Right((Unfalsified, i)))
          case s => s

        }
      }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String = ???

}

case class Gen[+A](sample: State[RNG, A], exhaustive: Stream[A]) {

  def map[B](f: A => B): Gen[B] = new Gen(sample.map(f), exhaustive.map(f))
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = new Gen(sample.map2(g.sample)(f), exhaustive.zipWith(g.exhaustive)(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = new Gen(sample.flatMap(v => f(v).sample), exhaustive.flatMap(v => f(v).exhaustive))
  def foreach[U](f: A => U) {
    sample.map(f)
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size flatMap { s => Gen.listOfN(s, this) }
  }
}

object Gen {

  def unit[A](a: => A): Gen[A] = {
    new Gen(State.unit(a), Stream.constant(a))
  }

  def boolean: Gen[Boolean] = {
    new Gen(State(RNG.boolean), Stream(true, false))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    new Gen(State(RNG.positiveInt).map { v => start + v % (stopExclusive - start) }, Stream.from(start))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    new Gen(State.sequence(List.fill(n)(g.sample)), Stream.empty)
  }

  def uniform: Gen[Double] = new Gen(State(RNG.double), Stream.empty)

  def choose(i: Double, j: Double): Gen[Double] = {
    new Gen(State(RNG.double).map { v => i + v * (j - i) }, Stream.empty)
  }

  def odd(start: Int, stopExclusive: Int): Gen[Int] = {
    choose(start, stopExclusive) map (v => if (v % 2 == 0) v - 1 else v)
  }

  def even(start: Int, stopExclusive: Int): Gen[Int] = {
    choose(start, stopExclusive) map (v => if (v % 2 != 0) v + 1 else v)
  }

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = {
    for {
      i <- choose(from, to)
      j <- if (i % 2 != 0) odd(from, to) else even(from, to)
    } yield (i, j)
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean flatMap (v => if (v) g1 else g2)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {

    //RNG.double generates values between 0 and 1
    val upTo = g1._2 / (g1._2 + g2._2)

    new Gen(State(RNG.double).flatMap(v => if (v < upTo) g1._1.sample else g2._1.sample), Stream.empty)
  }

}