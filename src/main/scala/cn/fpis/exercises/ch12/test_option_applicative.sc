package cn.fpis.exercises.ch12

object test_option_applicative {

  //In applicatives, functions are optionals, comparing them to a Functor (map)
  def apply[A, B](oab: Option[A => B])(oa: Option[A]): Option[B] = {

    (oab, oa) match {

      case (Some(f), Some(v)) => Some(f(v))
      case _ => None

    }

  }                                               //> apply: [A, B](oab: Option[A => B])(oa: Option[A])Option[B]

  Validations.validate("Guillermo Szeliga", "2011-11-01", "9988770000")
                                                  //> res0: cn.fpis.exercises.ch12.Validation[String,cn.fpis.exercises.ch12.WebFor
                                                  //| m] = Success(WebForm(Guillermo Szeliga,Tue Nov 01 00:00:00 CET 2011,99887700
                                                  //| 00))
  Validations.validate("", "201111-01", "88770000")
                                                  //> res1: cn.fpis.exercises.ch12.Validation[String,cn.fpis.exercises.ch12.WebFor
                                                  //| m] = Failure(Name cannot be empty,Vector(Birthdate must be in the form yyyy-
                                                  //| MM-dd, Phone number must be 10 digits))

  Traverse.listTraverse.zipWithIndex(List("a", "b", "c"))
                                                  //> res2: List[(String, Int)] = List((a,0), (b,1), (c,2))
  Traverse.listTraverse.reverse(List("a", "b", "c"))
                                                  //> res3: List[String] = List(c, b, a)
  Traverse.listTraverse.foldLeft(List("a", "b", "c"))(0)((acc, s) => acc + 1)
                                                  //> res4: Int = 3
}