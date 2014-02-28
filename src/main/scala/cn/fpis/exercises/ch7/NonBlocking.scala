package cn.fpis.exercises.ch7

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Callable
import scala.util.Either

object NonBlocking {

  trait Future[+A] {
    private[ch7] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](ex: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)

      p(ex) { v =>
        ref.set(v)
        latch.countDown
      }

      latch.await
      ref.get
    }

    def unit[A](v: A): Par[A] = {
      ex =>
        new Future[A] {
          def apply(k: A => Unit): Unit = k(v)
        }
    }

    def eval(ex: ExecutorService)(r: => Unit): Unit = {

      ex.submit(new Callable[Unit] {
        def call = r
      })

    }

    def fork[A](v: => Par[A]): Par[A] = {
      ex =>
        new Future[A] {
          def apply(k: A => Unit) = {
            eval(ex) {
              v(ex)(k)
            }
          }
        }
    }

    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = {

      ex =>
        {
          new Future[C] {
            def apply(k: C => Unit) = {

              var va: Option[A] = None
              var vb: Option[B] = None

              val actor = Actor[Either[A, B]](ex) {

                case Left(v) => {
                  if (vb.isDefined) k(f(v, vb.get))
                  else va = Some(v)
                }
                case Right(v) => {
                  if (va.isDefined) k(f(va.get, v))
                  else vb = Some(v)
                }
              }

              pa(ex) { v => actor ! Left(v) }
              pb(ex) { v => actor ! Right(v) }

            }
          }
        }

    }

    def map[A, B](p: Par[A])(f: A => B): Par[B] = {
      ex =>
        new Future[B] {
          def apply(k: B => Unit) = {
            p(ex) { v =>
              eval(ex)(k(f(v)))
            }
          }
        }
    }

    def async[A](v: => A): Par[A] = fork(unit(v))
    def asyncF[A, B](f: A => B): A => Par[B] = {
      a => async(f(a))
    }

    def sequenceRight[A](l: List[Par[A]]): Par[List[A]] = {

      l match {
        case Nil => unit(Nil)
        case h :: tail => map2(h, fork(sequenceRight(tail)))(_ :: _)
      }
    }

    def sequenceBalanced[A](l: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {

      if (l.isEmpty) unit(Vector())
      else if (l.size == 1) {
        map(l.head) { v => Vector(v) }
      } else {
        val (v1, v2) = l.splitAt(l.size / 2)
        map2(sequenceBalanced(v1), sequenceBalanced(v2))(_ ++ _)
      }

    }

    def sequence[A](l: List[Par[A]]): Par[List[A]] = {
      map(sequenceBalanced(l.toIndexedSeq))(_.toList)
    }

    def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = {
      sequence(l.map(asyncF(f)))
    }

    def parMap[A, B](l: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] = {
      sequenceBalanced(l.map(asyncF(f)))
    }

    def choice[A](pIf: Par[Boolean])(pTrue: Par[A], pFalse: Par[A]): Par[A] = {
      ex =>
        new Future[A] {
          def apply(f: A => Unit) = {
            pIf(ex) { r =>
              if (r) eval(ex)(pTrue(ex)(f))
              else eval(ex)(pFalse(ex)(f))
            }

          }
        }
    }

    def choiceN[A](p: Par[Int])(l: List[Par[A]]): Par[A] = {
      ex =>
        new Future[A] {
          def apply(f: A => Unit) = {
            p(ex) { i =>
              eval(ex) { l(i)(ex)(f) }
            }
          }
        }
    }

    def choiceWithChoiceN[A](pIf: Par[Boolean])(pTrue: Par[A], pFalse: Par[A]): Par[A] = {

      ex =>
        new Future[A] {
          def apply(f: A => Unit) = {
            choiceN(map(pIf)(v => if (v) 1 else 0))(List(pFalse, pTrue))
          }
        }

    }

    def choiceMap[K, V](pk: Par[K])(pv: Map[K, Par[V]]): Par[V] = {
      ex =>
        new Future[V] {
          def apply(f: V => Unit) = {
            pk(ex) { k =>
              eval(ex) { pv(k)(ex)(f) }
            }
          }
        }
    }

    //Also called bind since actually bound to Pars
    def flatMap[A, B](p: Par[A])(g: A => Par[B]): Par[B] = {

      ex =>
        new Future[B] {
          def apply(f: B => Unit) = {
            p(ex) { v => eval(ex) { g(v)(ex)(f) } }
          }
        }

    }

    def choiceWithFlatMap[A](pIf: Par[Boolean])(pTrue: Par[A], pFalse: Par[A]): Par[A] = {
      flatMap(pIf) { v => if (v) pTrue else pFalse }
    }

    def choiceNWithFlatMap[A](p: Par[Int])(l: List[Par[A]]): Par[A] = {
      flatMap(p)(l)
    }

    def join[A](p: Par[Par[A]]): Par[A] = {
      ex =>
        new Future[A] {
          def apply(f: A => Unit) = {
            map(p) { v =>
              v(ex)(f)
            }
          }
        }
    }

    def flatMapWithJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] = {
      join(map(p)(f))
    }

    def joinWithFlapMap[A](p: Par[Par[A]]): Par[A] = {
      flatMap(p) { v => v }
    }

  }
}