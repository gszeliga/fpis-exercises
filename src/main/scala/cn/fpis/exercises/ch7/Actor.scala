package cn.fpis.exercises.ch7

import java.util.concurrent.ExecutorService
import java.util.concurrent.Callable
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

final case class Actor[A](strategy: Strategy)(handler: A => Unit, onError: Throwable => Unit = throw (_)) {

  private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]

  //Head and tail share the same node instance at the beginning of the process
  private val tail = new AtomicReference(new Node[A])
  private val suspended = new AtomicInteger(1)
  private val head = new AtomicReference(tail.get)

  def !(a: A) = {
    val node = new Node(a)
    head.getAndSet(node).lazySet(node)
    trySchedule
  }

  private def trySchedule: Unit = { if (suspended.compareAndSet(1, 0)) schedule }

  private def schedule: Unit = { strategy(act) }

  private def act: Unit = {

    val t = tail.get
    val node = batchHandle(t, 1024)

    //If we made any progress and exhausted batch 
    if (t ne node) {
      node.a = null.asInstanceOf[A] //Remove value from latest consumed node and reset it as the initial node again
      tail.lazySet(node)
      schedule
    } else {
      suspended.set(1)
      //If we happened to have something in the meantime then we re schedule again, otherwise, nothing
      if (node.get ne null) trySchedule
    }

  }

  @tailrec
  private def batchHandle(t: Node[A], i: Int): Node[A] = {

    val node = t.get

    //'ne' checks for referential equality, since null will always have the same reference, it's more than proper to use it
    if (node ne null) {
      try {
        handler(node.a)
      } catch {
        case ex: Throwable => onError(ex)
      }
      if (i > 0) batchHandle(node, i - 1) else node
    } else t

  }

}

object Actor {

  def apply[A](ex: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw (_)): Actor[A] = {
    Actor(Strategy.fromExecutor(ex))(handler, onError)
  }

}

trait Strategy {
  def apply[A](a: => A): () => A
}

object Strategy {

  def fromExecutor(ex: ExecutorService): Strategy = new Strategy {
    def apply[A](a: => A) = {
      val future = ex submit (new Callable[A] { def call = a })
      () => future.get
    }
  }

  //This code runs immediately and using the current thread
  def sequential: Strategy = new Strategy {
    def apply[A](a: => A) = {
      val result = a
      () => result
    }
  }

}