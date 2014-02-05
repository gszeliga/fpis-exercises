package ch.fpis.exercises.ch4

object test {
	PatternExample.sequence(List(Some(1), Some(2), Some(3)))
                                                  //> res0: ch.fpis.exercises.ch4.Option[List[Int]] = Some(List(1, 2, 3))
	PatternExample.traverse(List(1, 2, 3))(x => Some(x + 5))
                                                  //> res1: ch.fpis.exercises.ch4.Option[List[Int]] = Some(List(6, 7, 8))
	
	
}