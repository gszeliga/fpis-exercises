package ch.fpis.exercises.ch5

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = {

    def loop(current: Stream[A], items: List[A]): List[A] = {
      if (current.isEmpty) items
      else loop(current.uncons.get._2, current.uncons.get._1 :: items)
    }

    loop(this, Nil)
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

}