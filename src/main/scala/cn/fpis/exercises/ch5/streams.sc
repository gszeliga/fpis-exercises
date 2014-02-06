package greeter

import ch.fpis.exercises.ch5.Stream

object streams {

 
  Stream(1, 2, 3)
  Stream(1, 2, 3) toList

  Stream(1, 2, 3) take (2) toList

  Stream(1, 2, 3) takeWhile (_ % 2 > 0) toList

  Stream(1, 2, 3) forAll (_ < 4)
  Stream(1, 2, 3) forAll (_ > 4)

  Stream(1, 2, 3).foldRight(0)(_ + _)
  Stream(1, 2, 3).exists(_ == 4)


	Stream.ones.take(5) toList

}