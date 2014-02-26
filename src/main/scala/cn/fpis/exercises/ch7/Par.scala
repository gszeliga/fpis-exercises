package cn.fpis.exercises.ch7

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](u: A): Par[A] = (e: ExecutorService) => new UnitFuture(u)

  // `map2` does not evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. 
  // We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    ex =>
      {
        new UnitFuture(f(a(ex).get, b(ex).get))
      }
  }

  def map2v2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = {
    map(product(pa, pb))(v => f(v._1, v._2))
  }

  def fork[A](v: => Par[A]): Par[A] = {
    ex =>
      {
        ex.submit(new Callable[A]() {
          def call: A = {
            v(ex).get
          }
        })
      }
  }

  def async[A](a: A): Par[A] = fork(unit(a))
  def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a))

  def product[A, B](pa: Par[A], pb: Par[B]): Par[(A, B)] = {
    ex =>
      {
        ex.submit(new Callable[(A, B)] {
          def call = {
            (pa(ex).get, pb(ex).get)
          }
        })

      }

  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    ex =>
      {
        ex.submit(new Callable[B] {
          def call = {
            f(pa(ex).get)
          }
        })
      }
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] = {

    l.foldRight(unit(List.empty[A]))((v, acc) => map2v2(v, acc)(_ :: _))

  }

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = {
    sequence(l.map(v => asyncF(f)(v)))
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val ops: List[Par[List[A]]] = l map asyncF(x => if (f(x)) List(x) else Nil)
    map(sequence[List[A]](ops))(_.flatten)
  }

  def par[A](z: A)(nums: List[A])(f: (A, A) => A): Par[A] = {

    val (l1, l2) = nums.splitAt(nums.length / 2)

    if (nums.size <= 1) nums.headOption.map(unit(_)).getOrElse(unit(z))
    else {
      map2v2(fork(par(z)(l1)(f)), fork(par(z)(l2)(f)))((a, b) => f(a, b))
    }

  }

  def sortPar[A](l: Par[List[A]])(implicit ord: Ordering[A]) = map(l)(_.sorted)

  //We're being forced to refer the ExecutorService, which is a little bit ugly for composition and computation delay
  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {

    ex =>
      {
        if (a(ex).get) ifTrue(ex)
        else ifFalse(ex)
      }

  }

  def choiceN[A](a: Par[Int])(l: List[Par[A]]): Par[A] = {
    ex =>
      {
        l(a(ex).get)(ex)
      }
  }

  def choiceNWithChooser[A](a: Par[Int])(l: List[Par[A]]): Par[A] = {
    chooser(a)(l)
  }

  def choiceUsingN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    choiceN(map(a) { v => if (v) 1 else 0 })(List(ifFalse, ifTrue))
  }

  def choiceMap[A, B](a: Par[A])(m: Map[A, Par[B]]): Par[B] = {
    ex =>
      {
        m(a(ex).get)(ex)
      }
  }

  def choiceMapWithChooser[A, B](a: Par[A])(m: Map[A, Par[B]]): Par[B] = {
    chooser(a)(m)
  }

  def chooser[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
    ex =>
      {
        run(ex)(f(run(ex)(a).get))
      }
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {

    ex =>
      {
        run(ex)(run(ex)(map(a)(f)).get())
      }
  }

  def flatMapWithJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
    join(map(a)(f))
  }

  def join[A](p: Par[Par[A]]): Par[A] = {
    ex => run(ex)(run(ex)(p).get())
  }

  def run[A](ex: ExecutorService)(p: Par[A]): Future[A] = p(ex)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def cancel(mayInterruptIfRunning: Boolean) = false
    def isCancelled = false
    def isDone = true
    def get(timeout: Long, unit: TimeUnit): A = get
  }

}
