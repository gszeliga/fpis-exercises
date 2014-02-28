package cn.fpis.exercises.ch7

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

object test_par {
  val ex = Executors.newCachedThreadPool()        //> ex  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPool
                                                  //| Executor@7b479feb

  val lst = List(1, 2, 3, 24, 5, 6, 7, 8)         //> lst  : List[Int] = List(1, 2, 3, 24, 5, 6, 7, 8)

  Par.run(ex)(Par.parFilter(lst)(_ > 4)).get      //> res0: List[Int] = List(24, 5, 6, 7, 8)

  Par.par(0)(lst)(_ + _)(ex).get                  //> res1: Int = 56
  Par.par(0)(lst)((a, b) => if (a > b) a else b)(ex).get
                                                  //> res2: Int = 24\

}