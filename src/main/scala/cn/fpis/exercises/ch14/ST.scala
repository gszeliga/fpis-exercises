package cn.fpis.exercises.ch14

/**
 * Created by guillermo on 8/01/15.
 */
sealed trait ST[S,A]{
  self =>

  //S represents the ability to mutate state, therefore, it must stay confined.
  //S works as an authorization to mutate or access the cell, but it serves no other purpose.
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

    //Since the RunnableSlengthT action is polymorphic in S , it's guaranteed to not make use of the
    // value that gets passed in. So it's actually completely safe to pass null !
    r[Null].run(null)._1
  }

}

//we will introduce a new trait that represents ST actions that are safe to run
//They don't expose STRef at all
trait RunnableST[A]
{
  //This is polimorphic since S will be supplied by the caller
  def apply[S]: ST[S,A]
}

sealed abstract class STArray[S,A](implicit manifest: Manifest[A]){

  ref =>

  protected def value: Array[A]

  def size: ST[S,Int] = ST(value.size)

  def write(i: Int, v: A):ST[S,Unit] = new ST[S,Unit] {
    def run(s:S) = {
      value(i)=v
      ((),s)
    }
  }

  def read(i:Int): ST[S, A] = ST(value(i))
  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int,A]): ST[S, Unit]= {
    xs.foldLeft(ST[S, Unit](())){case (acc, (i,v)) => {
      //We compose a new ST using flatMap (this is the only way to compose a final ST
      //with all the 'write' operations on it, disregarding the actual incoming parameter
      acc.flatMap(_ => write (i,v))
    }}
  }

  def swap(i: Int, j: Int): ST[S,Unit] = {
    for {
      x <- read(i)
      y <- read(j)
      _ <- write(j,x)
      _ <- write(i,y)
    } yield ()
  }

}

object STArray{
  def apply[S,A](sz: Int, v:A): ST[S,STArray[S,A]] = {
    ST(new STArray[S,A] {
      lazy val value= Array.fill(sz)(v)
    })
  }

  def apply[S,A](sz: Int): ST[S,STArray[S,A]] = {
    ST(new STArray[S,A] {
      lazy val value= new Array(sz)
    })
  }
}