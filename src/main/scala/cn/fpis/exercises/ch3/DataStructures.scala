package cn.fpis.exercises.ch3

import scala.annotation.tailrec

/**
 * Created by guillermo on 11/11/15.
 */
class DataStructures {

  @tailrec
  final def foldLeft[A,B](l: List[A],z: B)(f: (B,A) => B): B = {

    l match {
      case Nil => z
      case h :: t => foldLeft(t,f(z,h))(f)
    }

  }

  final def foldRight[A,B](l: List[A],z: B)(f: (A,B) => B): B = {

    l match {
      case Nil => z
      case h :: t => f(h,foldRight(t,z)(f))
    }

  }

  final def length[A](l:List[A]):Int = {
    foldLeft(l,0)((c,_) => c+1)
  }

  final def reverse[A](l: List[A]): List[A] = {
    foldRight(l,List.empty[A])(_ +: _)
  }

  @tailrec
  final def drop[A](l: List[A],n: Int): List[A] = {
    if(n == 0) l
    else drop(l.tail,n-1)
  }

  final def dropWhile[A](l:List[A])(f: A => Boolean): List[A] = {

    @tailrec
    def doDrop(source: List[A],tmp: List[A]): List[A] =
    {
      source match {
        case Nil => tmp
        case h :: t if f(h) => doDrop(t, h +: tmp)
        case h :: t if !f(h) => doDrop(t,tmp)
      }
    }

    doDrop(l,List.empty)

  }

}
