package ch.fpis.exercises.ch4

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(102); val res$0 = 
	PatternExample.sequence(List(Some(1), Some(2), Some(3)));System.out.println("""res0: ch.fpis.exercises.ch4.Option[List[Int]] = """ + $show(res$0));$skip(58); val res$1 = 
	PatternExample.traverse(List(1, 2, 3))(x => Some(x + 5));System.out.println("""res1: ch.fpis.exercises.ch4.Option[List[Int]] = """ + $show(res$1))}
	
	
}
