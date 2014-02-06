package ch.fpis.exercises.ch5

import scala.annotation.tailrec

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = {

    @tailrec
    def loop(current: Stream[A], items: List[A]): List[A] = {
      if (current.isEmpty) items
      else loop(current.uncons.get._2, current.uncons.get._1 :: items)
    }

    loop(this, Nil).reverse
  }

  def foreach[B](f: A => B): B = {

    this.uncons match {
      case Some((v, s)) => { f(v); s.foreach(f) }
      case _ => Unit.asInstanceOf[B]
    }

  }

  def take(n: Int): Stream[A] = {

    if (n == 0) Stream()
    else {
      uncons match {
        case Some((v, s)) if n == 1 => Stream.cons(v, Stream.empty)
        case Some((v, s)) => Stream.cons(v, s.take(n - 1))
        case _ => Stream()
      }
    }
  }

  def takeWhile(f: A => Boolean): Stream[A] = {

    uncons match {

      case Some((v, s)) if f(v) => Stream.cons(v, s.takeWhile(f))
      case _ => Stream.empty
    }

  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    uncons match {
      case Some((v, s)) => f(v, s.foldRight(z)(f))
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def forAll(f: A => Boolean): Boolean = {

    uncons match {

      case Some((v, s)) if s.isEmpty => f(v)
      case Some((v, s)) if f(v) => s.forAll(f)
      case _ => false
    }

  }

  def forAll2(p: A => Boolean): Boolean = {

    foldRight(true)((a, b) => p(a) && b)
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((v, acc) => Stream.cons(f(v), acc))
  }

  def append[B >: A](s: Stream[B]): Stream[B] = {

    this.uncons match {
      case Some((v, t)) => Stream.cons(v, t.append(s))
      case None => s
    }

  }

  //Does the same thing as append but more concise
  def appendWithFold[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = {

    foldRight(Stream.empty[B])((v, acc) => f(v) append acc)

  }

  def unfoldMap[B](f: A => B): Stream[B] = {
    Stream.unfold(this) { _.uncons map { x => (f(x._1), x._2) } }
  }

  def unfoldTake(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (stream, p) if p > 0 => stream.uncons map {
        case (v, s) => (v, (s, p - 1))
      }
      case _ => None
    }
  }

  def unfoldTakeWhile(f: A => Boolean): Stream[A] = {

    Stream.unfold(this) {
      _.uncons match {

        case Some((v, s)) if (f(v)) => Some(v, s)
        case _ => None
      }
    }

  }

  def zip[B, C](s: Stream[B]): Stream[(A,B)] = zipWith(s)((_, _))

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = {

    Stream.unfold((this, s)) {
      case (sa, sb) => {

        (sa.uncons, sb.uncons) match {

          case (Some((ha, ta)), Some((hb, tb))) => Some(f(ha, hb), (ta, tb))
          case _ => None
        }

      }
    }

  }

}

object Stream {

  def empty[A]: Stream[A] = new Stream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Stream[A] {
    lazy val uncons = Some(hd, tl)
  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) Stream.empty
    else Stream.cons(as.head, apply(as.tail: _*))
  }

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //corecursive function
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {

    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  }

}