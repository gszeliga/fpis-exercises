package greeter

import ch.fpis.exercises.ch5.Stream

object streams {

  Stream(1, 2, 3)                                 //> res0: ch.fpis.exercises.ch5.Stream[Int] = ch.fpis.exercises.ch5.Stream$$anon$
                                                  //| 2@b23b25c

  Stream(1, 2, 3) map (_ + 2) toList              //> res1: List[Int] = List(3, 4, 5)

  Stream(1, 2, 3) unfoldMap (_ + 2) toList        //> res2: List[Int] = List(3, 4, 5)

  Stream(1, 2, 3) toList                          //> res3: List[Int] = List(1, 2, 3)

  Stream(1, 2, 3) take (2) toList                 //> res4: List[Int] = List(1, 2)

  Stream(1, 2, 3) unfoldTake (2) toList           //> res5: List[Int] = List(1, 2)

  Stream(1, 2, 3) takeWhile (_ % 2 > 0) toList    //> res6: List[Int] = List(1)
  
  Stream(1, 2, 3) unfoldTakeWhile (_ % 2 > 0) toList
                                                  //> res7: List[Int] = List(1)

  Stream(1, 2, 3) forAll (_ < 4)                  //> res8: Boolean = true
  Stream(1, 2, 3) forAll (_ > 4)                  //> res9: Boolean = false

  Stream(1, 2, 3).foldRight(0)(_ + _)             //> res10: Int = 6
  Stream(1, 2, 3).exists(_ == 4)                  //> res11: Boolean = false

  Stream.ones.take(5) toList                      //> res12: List[Int] = List(1, 1, 1, 1, 1)

  Stream.constant("Hell yea!").take(5) toList     //> res13: List[String] = List(Hell yea!, Hell yea!, Hell yea!, Hell yea!, Hell 
                                                  //| yea!)

  Stream.from(5).take(6) toList                   //> res14: List[Int] = List(5, 6, 7, 8, 9, 10)

  Stream.unfold(1)(x => Some(x, x + 1)) take (5) toList
                                                  //> res15: List[Int] = List(1, 2, 3, 4, 5)

  Stream.unfold("Hell Yeah!")(y => Some(y, y)) take (6) toList
                                                  //> res16: List[String] = List(Hell Yeah!, Hell Yeah!, Hell Yeah!, Hell Yeah!, H
                                                  //| ell Yeah!, Hell Yeah!)

	Stream(1, 2, 3) foreach println           //> 1
                                                  //| 2
                                                  //| 3

	Stream(1, 2, 3) append (Stream(4, 5, 6)) toList
                                                  //> res17: List[Int] = List(1, 2, 3, 4, 5, 6)
  
  Stream(1, 2, 3).zipWith (Stream(4, 5, 6)) ((_,_)) toList
                                                  //> res18: List[(Int, Int)] = List((1,4), (2,5), (3,6))
  
  Stream(1, 2, 3) zip Stream("Hola", "Que", "Tal") toList
                                                  //> res19: List[(Int, String)] = List((1,Hola), (2,Que), (3,Tal))

}