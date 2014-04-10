package cn.fpis.exercises.ch10

object Ordered {

  sealed trait Order
  case class Current(v: Int) extends Order
  case object Init extends Order
  case object Greater extends Order

  val orderMonoid = new Monoid[Order] {
    def op(m1: Order, m2: Order): Order = {

      (m1, m2) match {
    	 
        case (Init, i @ _) => i
        case (Current(v1), c2 @ Current(v2)) => {
          if (v1 <= v2) c2
          else Greater
        }
        case _ => Greater
      }

    }
    def zero: Order = Init
  }

  
  def ordered(l: List[Int]): Boolean = {
    
    Monoid.foldMap(l, orderMonoid)(Current(_)) match {
      case Greater => false
      case _ => true
    }
  }  
}