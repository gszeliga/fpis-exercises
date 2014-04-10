package cn.fpis.exercises.ch10

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
     
  WC.count("hola que tal")                        //> res0: Int = 3

	WC.count("hola que tal como vamos")       //> res1: Int = 5
	
	List(1,2,3,4).tails.toList                //> res2: List[List[Int]] = List(List(1, 2, 3, 4), List(2, 3, 4), List(3, 4), Li
                                                  //| st(4), List())
	Ordered.ordered(List(1,2,3,4))            //> res3: Boolean = true
	Ordered.ordered(List(123,125,127))        //> res4: Boolean = true
	Ordered.ordered(List(123,143,125,127))    //> res5: Boolean = false
}