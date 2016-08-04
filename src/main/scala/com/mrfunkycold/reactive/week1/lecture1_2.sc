abstract class JSON
case class JSeq (elems: List[JSON]) extends JSON
case class JObj (bindings: Map[String, JSON]) extends JSON
case class JNum (num: Double) extends JSON
case class JStr (str: String) extends JSON
case class JBool (b: Boolean) extends JSON
case object JNull extends JSON

def show(json: JSON): String = json match {
  case JSeq(elems) => "[" + (elems map show mkString ",\n") + "]"
  case JObj(bindings) => {
    val assoc = bindings map { case(s, j) => s + ": " + show(j)}

    "{\n" + (assoc mkString ",\n") + "\n}"
  }
  case JNum(num) => num.toString
  case JStr(str) => "\"" + str + "\""
  case JBool(b) => b.toString
  case JNull => "null"
  case _ => ""
}

val data = JObj(
  Map( "firstName" -> JStr("John"),
       "lastName" -> JStr("Smith"),
       "address" -> JObj(Map(
         "streetAddress" -> JStr("21 2nd street"),
         "state" -> JStr("NY"),
         "postalCode" -> JStr("10021")
       )),
       "phoneNumbers" -> JSeq(List(
         JObj(Map(
           "type" -> JStr("home"),
           "number" -> JStr("610-731-6827")
         )),
         JObj(Map(
           "type" -> JStr("fax"),
           "number" -> JStr("203-423-3228")
         ))
       ))
  )
)

show(data)