package cn.fpis.exercises.ch10

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lSub: String, words: Int, rSub: String) extends WC

object WC {
  val wcMonoid = new Monoid[WC] {
    def op(wc1: WC, wc2: WC) = {

      (wc1, wc2) match {
        case (Stub(l), Stub(r)) => Stub(l + r)
        case (Stub(l), Part(sl, wc, sr)) => Part(sl + l, wc, sr)
        case (Part(sl, wc, sr), Stub(r)) => Part(sl, wc, sr + r)
        case (Part(sl1, wc1, sr1), Part(sl2, wc2, sr2)) => {
          Part(sl1, wc1 + (if ((sr1 + sl2).isEmpty()) 0 else 1) + wc2, sr2)
        }
      }

    }
    def zero = Stub("")
  }

  def count(s: String) = {

    def wc(ch: Char): WC = {
      if (ch.isWhitespace) {
        Part("", 0, "")
      } else Stub(ch.toString)
    }

    def unstub(s: String) = s.length min 1
    
    Monoid.foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l,wc, r) => unstub(l) + wc + unstub(r)
    }
  }

}