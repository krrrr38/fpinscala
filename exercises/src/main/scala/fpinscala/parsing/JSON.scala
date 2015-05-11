package fpinscala.parsing

//{
//  "firstName": "John",
//  "lastName": "Smith",
//  "isAlive": true,
//  "age": 25,
//  "address": {
//    "streetAddress": "21 2nd Street",
//    "city": "New York",
//    "state": "NY",
//    "postalCode": "10021-3100"
//  },
//  "phoneNumbers": [
//    {
//      "type": "home",
//      "number": "212 555-1234"
//    },
//    {
//      "type": "office",
//      "number": "646 555-4567"
//    }
//  ],
//  "children": [],
//  "spouse": null
//}
trait JSON

object JSON {
  /**
   *  An empty value, using the word null
   */
  case object JNull extends JSON

  /**
   * a signed decimal number that may contain a fractional part and may use exponential E notation.
   * JSON does not allow non-numbers like NaN, nor does it make any distinction between integer and floating-point.
   * (Even though JavaScript uses a double-precision floating-point format for all its numeric values, other languages
   * implementing JSON may encode numbers differently)
   * @param get
   */
  case class JNumber(get: Double) extends JSON

  /**
   * a sequence of zero or more Unicode characters.
   * Strings are delimited with double-quotation marks and support a backslash escaping syntax.
   * @param get
   */
  case class JString(get: String) extends JSON

  /**
   * either of the values true or false
   * @param get
   */
  case class JBool(get: Boolean) extends JSON

  /**
   * an ordered list of zero or more values, each of which may be of any type.
   * Arrays use square bracket notation with elements being comma-separated.
   * @param get
   */
  case class JArray(get: IndexedSeq[JSON]) extends JSON

  /**
   * an unordered collection of name/value pairs where the names (also called keys) are strings.
   * Since objects are intended to represent associative arrays,[10] it is recommended, though not required,[11]
   * that each key is unique within an object. Objects are delimited with curly brackets and use commas
   * to separate each pair, while within each pair the colon ':' character separates the key or name from its value.
   * @param get
   */
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def tr(value: String) = trim(string(value))

    val jnull: Parser[JSON] = tr("null").map(_ => JNull)
    val jbool: Parser[JSON] = tr("true") or tr("false") map (bool => JBool(bool == "true"))
    val jnumber: Parser[JSON] = number map (v => JNumber(v.toDouble))
    // TODO consider for escape sequence
    val jstring: Parser[JSON] = tr("\"") ~> regex(".*".r) <~ tr("\"") map JString
    val jconst: Parser[JSON] = jnull | jbool | jnumber | jstring
    def jobj: Parser[JSON] = tr("{") ~> (jstring ** tr(":") ** jvalue) rep tr(",") <~ tr("}") map { kvs =>
      val obj = kvs.foldLeft(Map.empty[String, JSON]) { case (m, ((key: JString, _), value)) =>
        m.updated(key.get, value)
      }
      JObject(obj)
    }
    def jarray: Parser[JSON] = tr("[") ~> jvalue.rep(tr(",")) <~ tr("]") map(ls => JArray(ls.toIndexedSeq))
    def jvalue: Parser[JSON] = jconst | jobj | jarray

    jvalue
  }
}


object JSONMain {
  def main(args: Array[String]) {
    val jsonParser = JSON.jsonParser(MyParsers)
    result(MyParsers.run(jsonParser)("1"))
    result(MyParsers.run(jsonParser)("true"))
    result(MyParsers.run(jsonParser)("{\"test\": 10.5}"))
    result(MyParsers.run(jsonParser)(" {  \"test\"  : 10.5  }  "))

    result(MyParsers.run(jsonParser)("truee"))
    result(MyParsers.run(jsonParser)(" {  \"test\"  : aaa  }  "))
  }

  private def result(res: Either[ParseError, JSON]): Unit = res match {
    case Left(err) =>
      println("- Failure")
      err.show
    case Right(json) =>
      println("+ Success")
      println(json)
  }
}