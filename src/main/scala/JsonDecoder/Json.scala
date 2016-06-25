package JsonDecoder

sealed trait Json
case class JNumber(value: BigDecimal) extends Json
case class JBool(value: Boolean) extends Json
case class JString(value: String) extends Json
case class JArray(a: Seq[Json]) extends Json
case class JObject(o: Map[String, Json]) extends Json
case object JNull extends Json

object Json {
  def pretty(json: Json): String = {
    json match {
      case JNumber(x) => x.toString
      case JBool(b) => b.toString
      case JString(s) => s
      case JArray(s) => s" [ ${s.map((json) => pretty(json)).mkString(", ")} ]"
      case JNull => "null"
      case JObject(map) => s"{ ${map.map { case (s: String, json: Json) => s ++ ": " ++ pretty(json) }.mkString(", ")} } "
      case _ => ""
    }
  }
}

case class User(name: String, age: Int) extends Json {
  def toJson(): String = {
    Json.pretty(this)
  }
}

object User {
  val userDecoder = {
    val nameDecoder = Decoder.chain(Decoder.field("name"), Decoder.string)
    val ageDecoder = Decoder.chain(Decoder.field("age"), Decoder.int)
    nameDecoder.map2(ageDecoder)((name, age) => User(name, age))
  }

  def decodeUser(json: Json): Option[User] = {
    userDecoder.decode(json)
  }
}

case class City(name: String)

object City {
  val cityDecoder = {
    val nameDecoder = Decoder.chain(Decoder.field("name"), Decoder.string)
    nameDecoder.map(name => City(name))
  }
}