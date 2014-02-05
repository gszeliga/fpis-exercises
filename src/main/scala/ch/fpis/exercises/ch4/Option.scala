package ch.fpis.exercises.ch4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(v) => Some(f(v))
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(v) => v
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def orElse[B >: A](opt: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse opt
  }

}

case class Some[A](val value: A) extends Option[A]
object None extends Option[Nothing]
