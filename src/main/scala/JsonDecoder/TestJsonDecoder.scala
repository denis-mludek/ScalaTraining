package JsonDecoder


object TestJsonDecoder {

  def tests: Unit = {
    val fooArray = JArray(Seq(JString("Foo"), JString("Bar")))

    val BazUserJson = JObject(
      Map(
        "name" -> JString("Baz"),
        "age" -> JNumber(30)
      )
    )

    println(Json.pretty(fooArray))
    println(User.decodeUser(BazUserJson))
  }
}
