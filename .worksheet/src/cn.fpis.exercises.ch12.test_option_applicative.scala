package cn.fpis.exercises.ch12

object test_option_applicative {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(314); 

	//In applicatives, functions are optionals, comparing them to a Functor (map)
  def apply[A, B](oab: Option[A => B])(oa: Option[A]): Option[B] = {

    (oab, oa) match {

      case (Some(f), Some(v)) => Some(f(v))
      case _ => None

    }

  };System.out.println("""apply: [A, B](oab: Option[A => B])(oa: Option[A])Option[B]""")}

}
