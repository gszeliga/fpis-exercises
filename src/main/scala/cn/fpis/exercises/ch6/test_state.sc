package cn.fpis.exercises.ch6

object test_state {

  val rng = RNG.simple(15)                        //> rng  : cn.fpis.exercises.ch6.RNG = cn.fpis.exercises.ch6.RNG$$anon$1@1b9e1916
                                                  //| 
          
	val s1 = State.unit(2)                    //> s1  : cn.fpis.exercises.ch6.State[Nothing,Int] = State(<function1>)
	val s2 = State.unit(3)                    //> s2  : cn.fpis.exercises.ch6.State[Nothing,Int] = State(<function1>)
	val s3 = new State[RNG, Int](_.nextInt)   //> s3  : cn.fpis.exercises.ch6.State[cn.fpis.exercises.ch6.RNG,Int] = State(<fu
                                                  //| nction1>)
        
	val s4 = new State[RNG, Int](x => x.nextInt)
                                                  //> s4  : cn.fpis.exercises.ch6.State[cn.fpis.exercises.ch6.RNG,Int] = State(<fu
                                                  //| nction1>)
	
	s4.map(v => v + 2).run(rng)._1            //> res0: Int = 5771235

	State.sequence(List(s3,s4)).run(rng)      //> res1: (List[Int], cn.fpis.exercises.ch6.RNG) = (List(5771233, -148167218),cn
                                                  //| .fpis.exercises.ch6.RNG$$anon$1@1efbd816)

}