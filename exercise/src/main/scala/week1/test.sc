object test {
  println("Welcome to the Scala worksheet")

  val f: PartialFunction[String, String] = { case "ping" => "pong" }
  f.isDefinedAt("ping")
  f.isDefinedAt("abc")
}