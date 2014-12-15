package cn.fpis.exercises.ch11

import scala.annotation.tailrec

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = {
    flatMap(ma)(a => unit(f(a)))
  }

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = {
    flatMap(ma)(a => map(mb)(b => f(a, b)))
  }

  def sequence[A](lma: List[M[A]]): M[List[A]] = {

    lma.foldRight(unit(List[A]())) { (ma, mla) =>
      map2(ma, mla)(_ :: _)
    }
  }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = {
    la.foldRight(unit(List[B]())) { (a, mb) =>
      map2(f(a), mb)(_ :: _)
    }
  }

  // For `List`, the `replicateM` function will generate a list of lists.
  // It will contain all the lists of length `n` with elements selected from the
  // input list.
  // For `Option`, it will generate either `Some` or `None` based on whether the
  // input is `Some` or `None`. The `Some` case will contain a list of length `n`
  // that repeats the element in the input `Option`.
  // The general meaning of `replicateM` is described very well by the
  // implementation `sequence(List.fill(n)(ma))`. It repeats the `ma` monadic value
  // `n` times and gathers the results in a single value, where the monad `M`
  // determines how values are actually combined.  

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    @tailrec
    def doReplicate(p: Int, mla: M[List[A]]): M[List[A]] = {
      if (p == 0) mla
      else {
        doReplicate(n - 1, map2(ma, mla)(_ :: _))
      }
    }

    doReplicate(n, unit(List[A]()))

  }



  def foldM[A,B](l: Stream[A])(z: B)(f: (B,A) => M[B]): M[B] = {
    l match {
      case h #:: t => flatMap(f(z,h))(foldM(t)(_)(f))
      case _ => unit(z)
    }
  }

  //Why using skip?
  def foldM_[A,B](l: Stream[A])(f: A => M[Unit]): M[Unit] = foldM(l)(())((a,v) => f(v))

  // Using `sequence` and the `List.fill` function of the standard library:
  def _replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  def cofactor[A, B](e: Either[M[A], M[B]]): M[Either[A, B]] = {

    e match {
      case Left(ma) => map(ma)(Left(_))
      case Right(ma) => map(ma)(Right(_))
    }

  }

  //Keisli arrows
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(g)

  def join[A](mma: M[M[A]]): M[A] = {
    flatMap(mma)(ma => ma)
  }

  def flatMapJoin[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
  def composeJoin[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => join(map(f(a))(b => g(b)))

  def when[A](b: Boolean)(fa: M[A]):M[Boolean] = {
    if(b) as(fa)(true) else unit(false)
  }

  def doWhile[A](ma: M[A])(cond: A => M[Boolean]): M[Unit] = {
    //Need to use double 'flatMap', otherwise it won't run the second time
    flatMap(ma){ a => flatMap(cond(a)){c =>
      println("Continue? " + c)
      if(c) doWhile(ma)(cond) else unit(())}
    }

/*    for{
      a <- ma
      ok <- cond(a)
      _ <- if(ok) doWhile(ma)(cond) else unit(())
    } yield ()*/

  }

  def forever[A,B](a: M[A]): M[B] = {
    lazy val la: M[B] = forever(a)
    flatMap(a)(_ => la)
  }

  def skip[A](ma: M[A]):M[Unit] = as(ma)(())

  def sequence_[A](s: Stream[M[A]]):M[Unit] = foreachM(s)(skip)
  def sequence_[A](s: M[A]*): M[Unit] = sequence_(s.toStream)

  def foreachM[A](l: Stream[A])(f: A => M[Unit]): M[Unit] = {
    foldM_(l){f(_)}
  }

  def as[A,B](ma: M[A])(b: B): M[B] = map(ma)(_ => b)

}