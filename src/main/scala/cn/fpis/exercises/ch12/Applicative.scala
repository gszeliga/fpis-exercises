package cn.fpis.exercises.ch12

import cn.fpis.exercises.ch11.Functor
import scala.annotation.tailrec
import cn.fpis.exercises.ch11.Monad
import java.util.Date
import cn.fpis.exercises.ch10.Foldable
import cn.fpis.exercises.ch10.Monoid
import cn.fpis.exercises.ch6.State

//A minimal implementation of Applicative must provide apply or map2.
trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(map(fa)(f.curried))(fb)
  }
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fab, fa)((a, b) => a(b))
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    map2(unit(f), fa)(_(_))
  }

  def sequenceMap2[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldLeft(unit(Map.empty[K, V])) { (a, v) =>
      map2(a, v._2) { (v1, v2) => v1 + (v._1 -> v2) }
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldLeft(unit(Map.empty[K, V])) {
      case (fm, (k, fv)) => apply(map(fm) { m => ((p: Map[K, V]) => p ++ m) })(map(fv)(v => Map(k -> v)))
    }
  }

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: A) = (self.unit(a), G.unit(a))
      override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  def unit[A](a: A): F[A]
}

//All monads are applicative functors
trait ApplicativeMonad[M[_]] extends Applicative[M] {

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    join(map(ma)(a => f(a)))
  }

  def join[A](m: M[M[A]]): M[A] = flatMap(m)(v => v)

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = {
    a: A => flatMap(f(a))(vb => g(vb))
  }

  override def apply[A, B](fab: M[A => B])(fa: M[A]): M[B] = {
    flatMap(fab)(f => flatMap(fa)(a => unit(f(a))))
  }

  def sequence[A](fas: List[M[A]]): M[List[A]] = {
    fas.foldRight(unit(List.empty[A])) { (a, b) => map2(a, b)(_ :: _) }
  }

  def traverse[A, B](as: List[A])(f: A => M[B]): M[List[B]] = {
    as.foldRight(unit(List.empty[B])) { (a, b) => map2(f(a), b)(_ :: _) }
  }

  override def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = {
    flatMap(ma)(a => map(mb)(f(a, _)))
  }

  override def map[A, B](m: M[A])(f: A => B): M[B] = {
    flatMap(m) { a => unit(f(a)) }
  }

  def replicateM[A](n: Int, fa: M[A]): M[List[A]] = {

    @tailrec
    def doReplicate(p: Int, fl: M[List[A]]): M[List[A]] = {
      if (p == 0) fl
      else {
        doReplicate(p - 1, map2(fa, fl)(_ :: _))
      }
    }

    doReplicate(n, unit(List.empty[A]))

  }

  def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] = {
    map2(ma, mb)((_, _))
  }

}

object Applicatives {

  def listApplicative = new Applicative[List] {
    override def apply[A, B](fab: List[A => B])(fa: List[A]): List[B] = {
      fab.flatMap(f => fa.map(f))
    }
    override def unit[A](a: A): List[A] = List(a)
  }

  def stateMonad[S] = new ApplicativeMonad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma flatMap f
  }

  def eitherMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
    def unit[A](a: => A): Either[E, A] = ???
    def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ???
  }

  //Applicative product??
  def product[G[_], F[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = ???

  //This type throws away type B and always gives A
  type Const[A, B] = A

  //Const[T,x] == T, and there relies the trick. This way, we can turn a Monoid into an Applicative (usually called 'monoidal functors')
  implicit def monoidApplicative[T](m: Monoid[T]) = new Applicative[({ type f[x] = Const[T, x] })#f] {
    def unit[A](a: A): T = m.zero
    override def apply[A, B](m1: T)(m2: T): T = m.op(m1, m2)
  }

}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]
case class Success[A](s: A) extends Validation[Nothing, A]
case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object Validations {

  private def validationApplicative[E] = new Applicative[({ type f[x] = Validation[E, x] })#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Success(v1), Success(v2)) => Success(f(v1, v2))
        case (Failure(e1, t1), Failure(e2, t2)) => Failure(e1, t1 ++ (e2 +: t2))
        case (_, v @ Failure(h, t)) => v
        case (v @ Failure(h, t), _) => v
      }
    }
    def unit[A](a: A): Validation[E, A] = new Success(a)
  }

  def validName(name: String): Validation[String, String] = {
    if (name != "") Success(name)
    else Failure("Name cannot be empty", Vector.empty)
  }

  def validateBirthdate(birthdate: String): Validation[String, Date] = {
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case e: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd", Vector.empty)
    }
  }

  def validatePhone(phone: String): Validation[String, String] = {
    if (phone.matches("[0-9]{10}"))
      Success(phone)
    else Failure("Phone number must be 10 digits", Vector.empty)
  }

  def validate(name: String, birthdate: String, phone: String): Validation[String, WebForm] = {

    val applicative = validationApplicative[String]

    applicative.apply(applicative.apply(applicative.apply(applicative.unit((WebForm(_, _, _)).curried))(validName(name)))(validateBirthdate(birthdate)))(validatePhone(phone))
  }

}

//A valid instance of Traverse must then override at least either sequence or traverse
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def sequence[M[_]: Applicative, A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  def traverse[M[_]: Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence(map(fa)(f))

  type Id[A] = A

  //Id applicative functor
  val idMonad = new ApplicativeMonad[Id] {
    def unit[A](a: A) = a
    override def flatMap[A, B](ma: A)(f: A => B): B = f(ma)
  }

  //here, map and traverse are the same operation (in the context of Id applicative functor) 
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    traverse[Id, A, B](fa)(f)(idMonad) // This is evidence that Id => Applicative[Id]
  }

  import Applicatives._
  import cn.fpis.exercises.ch6.State

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
    traverse[({ type f[x] = Const[B, x] })#f, A, Nothing](as)(f)(monoidApplicative(mb))
  }

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = {
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(stateMonad)
  }

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] = {

    import State._

    traverseS(ta) { a =>

      /*      for {
        v <- get[Int]
        _ <- set(v + 1)
      } yield (a, v)*/

      get[Int].flatMap(v => set(v + 1).map(_ => (a, v)))

    }.run(0)._1
  }

  override def toList[A](fa: F[A]): List[A] = {

    import State._

    traverseS(fa) { a =>
      get[List[A]].flatMap(l => set(a :: l))
    }.run(List.empty[A])._2.reverse //we just want the final state

  }

  //Abstract expression of all previous one: We get the current state, compute the next state, set it, and yield some value
  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = {

    import State._

    traverseS(fa) { a =>
      for {
        cs <- get[S] //get current state
        (nv, ns) = f(a, cs) //apply current value with current state
        _ <- set(ns) //save new state
      } yield nv //return new value
    }.run(s)
  }

  def mapAccumToList[A](fa: F[A]): List[A] = {
    mapAccum(fa, List.empty[A])((a, l) => ((), a :: l))._2
  }

  def mapAccumZipWithIndex[A](ta: F[A]): F[(A, Int)] = {
    mapAccum(ta, 0)((a, i) => ((a, i), i + 1))._1
  }

  def reverse[A](fa: F[A]): F[A] = {
    mapAccum(fa, toList(fa).reverse) { (_, l) => (l.head, l.tail) }._1
  }

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = {
    mapAccum(fa, z) { (v, acc) => (() , f(acc, v)) }._2
  }

  def zipL[A,B](fa: F[A], fb: F[B]):F[(A,Option[B])]={
    (mapAccum(fa, toList(fb)){
      case (element,Nil) => ((element, Option.empty[B]), Nil)
      case (element, h :: t) => ((element, Some(h)),t)
    })._1
  }

  def zipR[A,B](fa: F[A],fb: F[B]):F[(Option[A],B)]={
    mapAccum(fb, toList(fa)){
      case (elem, Nil) => ((Option.empty[A], elem), Nil)
      case (elem, h :: t) => ((Some(h), elem), t)
    }._1
  }

  def fuse[M[_], N[_],A,B](fa: F[A]) (f: A => M[B], g: A => N[B])(M: Applicative[M], N:Applicative[N]): (M[F[B]], N[F[B]])= {

    //Here creates a single type composed by two higher-kinded types as f[x] = (M[x], N[x])
    traverse[({type f[x] = (M[x], N[x])})#f, A, B](fa)(a => (f(a), g(a)))(M product N)

  }


}

object Traverse {
  val listTraverse = new Traverse[List] {

    override def traverse[M[_], A, B](fa: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] = {

      fa.foldRight(M.unit(List.empty[B])) { (v, acc) =>
        val v1 = M.map2(f(v), acc)(_ :: _)
        v1
      }
    }

  }
}

