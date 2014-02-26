package cn.fpis.exercises.ch7

object test_nonblocking {

  val p = NonBlocking.Par.unit {

    2 + 2

  }                                               //> p  : java.util.concurrent.ExecutorService => cn.fpis.exercises.ch7.NonBlocki
                                                  //| ng.Future[Int] = <function1>

  NonBlocking.Par.fork {
    val i = 2 + 2
    NonBlocking.Par.unit(i)
  }                                               //> res0: java.util.concurrent.ExecutorService => cn.fpis.exercises.ch7.NonBlock
                                                  //| ing.Future[Int] = <function1>
}