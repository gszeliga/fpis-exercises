package cn.fpis.exercises.ch10

trait Monoid[A] {
  def op(m1: A, m2: A): A
  def zero: A
}

//A monoid is a type together with an associative binary operation (op) which has an identity element (zero)
object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(m1: String, m2: String) = m1 + m2
    def zero = ""
  }

  val intMonoid = new Monoid[Int] {
    def op(m1: Int, m2: Int) = m1 + m2
    def zero = 0
  }

  val wordMonoid = new Monoid[String] {
    def op(m1: String, m2: String) = (m1.trim + " " + m2.trim).trim
    def zero = ""
  }

  def dual[A](m: Monoid[A]) = new Monoid[A] {
    def op(m1: A, m2: A) = m.op(m2, m1)
    def zero = m.zero
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(m1: Option[A], m2: Option[A]) = m1 orElse m2
    def zero = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(m1: A => A, m2: A => A) = v => m2(m1(v)) //same as m1 compose m2
    def zero = v => v
  }

  def concatenate[A](l: List[A], m: Monoid[A]): A = l.foldLeft(m.zero)(m.op)

  def foldMap[A, B](l: List[A], m: Monoid[B])(f: A => B): B = {
    l.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  def foldMapV[A, B](s: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {

    if (s.length == 0) m.zero
    else if (s.length == 1) f(s(0))
    else {
      val (s1, s2) = s.splitAt(s.length / 2)
      m.op(foldMapV(s1, m)(f), foldMapV(s2, m)(f))
    }

  }

  def foldRight[A, B](l: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(l, endoMonoid[B])(v => f(v, _))(z)
  }

  def foldLeft[A, B](l: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(l, dual(endoMonoid[B]))(v => f(_, v))(z)
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(m1: (A, B), m2: (A, B)): (A, B) = {
      (A.op(m1._1, m2._1), B.op(m1._2, m2._2))
    }

    def zero: (A, B) = (A.zero, B.zero)
  }

  // There is no correct `Either` monoid. The trouble lies in providing a `zero`.
  // Should the `zero` be a `Left` or a `Right`? Also, what happens when we combine
  // both a `Left` and a `Right`? It's not possible to make an arbitrary decision
  // about that and still satisfy the monoid laws.

  def coproductMonoid[A, B](A: Monoid[A],
    B: Monoid[B]): Monoid[Either[A, B]] = new Monoid[Either[A, B]] {

    def op(m1: Either[A, B], m2: Either[A, B]): Either[A, B] = ???
    def zero: Either[A, B] = ???

  }

  // But it is possible to define a monoid coproduct using a slightly different
  // data structure:  
  sealed trait These[+A, +B]
  case class This[A](a: A) extends These[A, Nothing]
  case class That[B](b: B) extends These[Nothing, B]
  case class Both[A, B](a: A, b: B) extends These[A, B]

  //Damn Runar did it again
  //http://blog.higher-order.com/blog/2014/03/19/monoid-morphisms-products-coproducts/

  def functionMonoid[A, B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(m1: A => B, m2: A => B) = a => m.op(m1(a), m2(a))
    def zero = a => m.zero
  }

  def mapMergeMonoid[K, V](m: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map.empty
    def op(m1: Map[K, V], m2: Map[K, V]) = {
      m1.map {
        case (k, v) => (k, m.op(v, m2.get(k) getOrElse m.zero))
      }
    }
  }

  def frequencyMap(strings: IndexedSeq[String]): Map[String, Int] = {
    foldMapV[String, Map[String, Int]](strings, mapMergeMonoid(intMonoid))(s => Map(s -> 1))
  }

}