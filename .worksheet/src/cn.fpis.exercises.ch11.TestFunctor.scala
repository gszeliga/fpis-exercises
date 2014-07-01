package cn.fpis.exercises.ch11

object TestFunctor {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(154); 

  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = ???
  };System.out.println("""listFunctor  : cn.fpis.exercises.ch11.Functor[List] = """ + $show(listFunctor ))}

}
