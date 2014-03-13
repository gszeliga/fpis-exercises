package cn.fpis.exercises.ch9

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser(P: Parsers): P.Parser[JSON] = {

    //implicits
    import P._

    def literal: Parser[JSON] = scope("literals") {
      string("null") as (JNull) or
        double.map(JNumber(_)) or
        unquotedString.map(JString(_)) or
        "true" as (JBool(true)) or
        "false" as (JBool(false))
    }

    def array = surrounded('[', ']') {
      value separatedBy (",") map (l => JArray(l.toIndexedSeq)) label ("arrays")
    }

    def keyval = unquotedString ** skipL(":", value) label ("key -> val")

    def obj = surrounded('{', '}') {
      keyval separatedBy (",") map (l => JObject(l.toMap)) label ("object")
    }

    def value: Parser[JSON] = literal or array or obj

    root(obj or array)
  }

}
