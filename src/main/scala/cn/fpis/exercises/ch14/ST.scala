package cn.fpis.exercises.ch14

/**
 * Created by guillermo on 8/01/15.
 */
sealed trait ST[S,A]{
  self =>

  //S represents the ability to mutate state, therefore, it must stay confined
  protected def run(s:S): (A,S)

  def map[B](f: A => B): ST[S,B] = new ST[S,B]{
    protected def run(s: S): (B, S) = {
      self.run(s) match {case (na,ns) => (f(na),ns)}
    }
  }

  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B]{
    protected def run(s: S) = {
      val (result, state) = self.run(s)
      f(result).run(state)
    }
  }

}

sealed trait STRef[S,A]{
  protected var cell:A

  def read: ST[S,A] = ST(cell)
  def write(a: => A): ST[S, Unit] = new ST[S,Unit]{
    protected def run(s: S) = {
      cell = a
      ((),s)
    }
  }
}

object STRef{
  def apply[S,A](v: => A): ST[S,STRef[S,A]] = ST(new STRef[S,A] {
    protected var cell = v
  })
}

object ST{
  def apply[S,A](a: => A) = {

    //We lazy eval it since we want to retrieve the by value expression only once
    lazy val memo = a

    new ST[S,A]{
      protected def run(s: S) = (memo,s)
    }

  }

  def runST[A](r: RunnableST[A]): A = {
    //Call apply on any polymorphic RunnableST by arbitrarily choosing a type for S
    //When we specify 'Null' we're actually building a RunnableST of that type. That's why we
    //can call 'run' right away
    r[Null].run(null)._1
  }

}

trait RunnableST[A]
{
  //This is polimorphic since S will be supplied by the caller
  def apply[S]: ST[S,A]
}