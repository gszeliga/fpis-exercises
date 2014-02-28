package cn.fpis.exercises.ch7

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

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

  def sum(x: Int, y: Int): NonBlocking.Par[Int] = {
    NonBlocking.Par.async(x + y)
  }                                               //> sum: (x: Int, y: Int)java.util.concurrent.ExecutorService => cn.fpis.exercis
                                                  //| es.ch7.NonBlocking.Future[Int]


	val ex = Executors.newCachedThreadPool    //> ex  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPool
                                                  //| Executor@30ec4a87

	val v1 = sum(5,7)                         //> v1  : java.util.concurrent.ExecutorService => cn.fpis.exercises.ch7.NonBlock
                                                  //| ing.Future[Int] = <function1>
  val v2 = sum(10,11)                             //> v2  : java.util.concurrent.ExecutorService => cn.fpis.exercises.ch7.NonBlock
                                                  //| ing.Future[Int] = <function1>
  val v3 = sum(5,7)                               //> v3  : java.util.concurrent.ExecutorService => cn.fpis.exercises.ch7.NonBlock
                                                  //| ing.Future[Int] = <function1>
  
	val v4 = NonBlocking.Par.map2(v1, v2)(_ + _)
                                                  //> v4  : java.util.concurrent.ExecutorService => cn.fpis.exercises.ch7.NonBlock
                                                  //| ing.Future[Int] = <function1>
	val v5 = NonBlocking.Par.map2(v4, v3)(_ + _)
                                                  //> v5  : java.util.concurrent.ExecutorService => cn.fpis.exercises.ch7.NonBlock
                                                  //| ing.Future[Int] = <function1>
  
  val keySelector = NonBlocking.Par.map(v5)(i => if(i > 40) "B" else "S")
                                                  //> keySelector  : java.util.concurrent.ExecutorService => cn.fpis.exercises.ch7
                                                  //| .NonBlocking.Future[String] = <function1>
  
  val bigger = NonBlocking.Par.unit("It's bigger!!")
                                                  //> bigger  : java.util.concurrent.ExecutorService => cn.fpis.exercises.ch7.NonB
                                                  //| locking.Future[String] = <function1>
  val smaller = NonBlocking.Par.unit("It's smaller!!")
                                                  //> smaller  : java.util.concurrent.ExecutorService => cn.fpis.exercises.ch7.Non
                                                  //| Blocking.Future[String] = <function1>
  
  val selector = NonBlocking.Par.choiceMap(keySelector)(Map("B" -> bigger, "S" -> smaller))
                                                  //> selector  : java.util.concurrent.ExecutorService => cn.fpis.exercises.ch7.No
                                                  //| nBlocking.Future[String] = <function1>
  
  
  NonBlocking.Par.run(ex)(selector)               //> res1: String = It's bigger!!

  ex.shutdownNow                                  //> res2: java.util.List[Runnable] = []

}