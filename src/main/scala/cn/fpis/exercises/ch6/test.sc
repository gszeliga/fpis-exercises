package cn.fpis.exercises.ch6

object test {
  val rng = RNG.simple(15)                        //> rng  : cn.fpis.exercises.ch6.RNG = cn.fpis.exercises.ch6.RNG$$anon$1@73995d80
                                                  //| 
  
  val (v, n) = rng.nextInt                        //> v  : Int = 5771233
                                                  //| n  : cn.fpis.exercises.ch6.RNG = cn.fpis.exercises.ch6.RNG$$anon$1@29e97f9f
  n.nextInt                                       //> res0: (Int, cn.fpis.exercises.ch6.RNG) = (-148167218,cn.fpis.exercises.ch6.R
                                                  //| NG$$anon$1@1be1a408)
  rng.positiveMax(50)(n)                          //> res1: (Int, cn.fpis.exercises.ch6.RNG) = (50,cn.fpis.exercises.ch6.RNG$$anon
                                                  //| $1@14d6a05e)
  val (v2,n2) = n.positiveInt(n)                  //> v2  : Int = 148167217
                                                  //| n2  : cn.fpis.exercises.ch6.RNG = cn.fpis.exercises.ch6.RNG$$anon$1@16ba8602
                                                  //| 
        
  n.positiveInt(n2)                               //> res2: (Int, cn.fpis.exercises.ch6.RNG) = (2140891119,cn.fpis.exercises.ch6.R
                                                  //| NG$$anon$1@2fb3f8f6)
  n.double(n2)                                    //> res3: (Double, cn.fpis.exercises.ch6.RNG) = (0.9969301144592464,cn.fpis.exer
                                                  //| cises.ch6.RNG$$anon$1@509df6f1)
  n.intDouble(n2)                                 //> res4: ((Int, Double), cn.fpis.exercises.ch6.RNG) = ((2140891119,0.3204478463
                                                  //| 1580114),cn.fpis.exercises.ch6.RNG$$anon$1@36b8bef7)
  n.intDoubleWithMap(n2)                          //> res5: ((Int, Double), cn.fpis.exercises.ch6.RNG) = ((2140891119,0.3204478463
                                                  //| 1580114),cn.fpis.exercises.ch6.RNG$$anon$1@2d342ba4)
  n.double3(n2)                                   //> res6: ((Double, Double, Double), cn.fpis.exercises.ch6.RNG) = ((0.9969301144
                                                  //| 592464,0.32044784631580114,-0.3797181476838887),cn.fpis.exercises.ch6.RNG$$a
                                                  //| non$1@744a6cbf)
  n.ints(6)(n2)                                   //> res7: (List[Int], cn.fpis.exercises.ch6.RNG) = (List(1543215055, 1538015203,
                                                  //|  788377272, -815438513, 688156510, 2140891119),cn.fpis.exercises.ch6.RNG$$an
                                                  //| on$1@6513cf0)
  n.ints2(6)(n2)                                  //> res8: (List[Int], cn.fpis.exercises.ch6.RNG) = (List(1543215055, 1538015203,
                                                  //|  788377272, -815438513, 688156510, 2140891119),cn.fpis.exercises.ch6.RNG$$an
                                                  //| on$1@3f77b3cd)
  n.ints2WithSequence(6)(n2)                      //> res9: (List[Int], cn.fpis.exercises.ch6.RNG) = (List(2140891119, 688156510, 
                                                  //| 815438512, 788377272, 1538015203, 1543215055),cn.fpis.exercises.ch6.RNG$$ano
                                                  //| n$1@a94884d)
        
  Int.MinValue                                    //> res10: Int(-2147483648) = -2147483648
  Int.MaxValue                                    //> res11: Int(2147483647) = 2147483647
}