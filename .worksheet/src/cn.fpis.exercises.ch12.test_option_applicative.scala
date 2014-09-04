package cn.fpis.exercises.ch12

object test_option_applicative {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(314); 

	//In applicatives, functions are optionals, comparing them to a Functor (map)
  def apply[A, B](oab: Option[A => B])(oa: Option[A]): Option[B] = {

    (oab, oa) match {

      case (Some(f), Some(v)) => Some(f(v))
      case _ => None

    }

  };System.out.println("""apply: [A, B](oab: Option[A => B])(oa: Option[A])Option[B]""");$skip(72); val res$0 = 

	Validations.validate("Guillermo Szeliga", "2011-11-01", "9988770000");System.out.println("""res0: cn.fpis.exercises.ch12.Validation[String,cn.fpis.exercises.ch12.WebForm] = """ + $show(res$0));$skip(51); val res$1 = 
	Validations.validate("", "201111-01", "88770000");System.out.println("""res1: cn.fpis.exercises.ch12.Validation[String,cn.fpis.exercises.ch12.WebForm] = """ + $show(res$1))}

}
