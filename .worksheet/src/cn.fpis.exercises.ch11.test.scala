package cn.fpis.exercises.ch11

//import cn.fpis.exercises.ch11.Monads

object test {
  case class Id[A](value: A) {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
    def map[B](f: A => B): Id[B] = unit(f(value))
  };import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(332); val res$0 = 

  for {
    a <- Id("Hello, ")
    b <- Id("Monad!")
  } yield a + b;System.out.println("""res0: cn.fpis.exercises.ch11.test.Id[String] = """ + $show(res$0));$skip(34); 

  val M = Monads.stateMonad[Int]

  type IntTuple[+A] = (Int, A);System.out.println("""M  : cn.fpis.exercises.ch11.Monad[[x]cn.fpis.exercises.ch6.State[Int,x]] = """ + $show(M ));$skip(150); 

  val f = new Functor[IntTuple] {
    def map[A, B](fa: IntTuple[A])(f: A => B): IntTuple[B] = (fa._1, f(fa._2))
  };System.out.println("""f  : cn.fpis.exercises.ch11.Functor[cn.fpis.exercises.ch11.test.IntTuple] = """ + $show(f ));$skip(27); val res$1 = 

	f.map((1,2))(a => a + 1);System.out.println("""res1: cn.fpis.exercises.ch11.test.IntTuple[Int] = """ + $show(res$1))}

}
