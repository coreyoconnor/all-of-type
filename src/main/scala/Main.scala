@AllOfType
object Foo {
  val x: String = "ASDAS"
  val y = "ASDAS"

  val all: Seq[String] = AllOfType.derive
}

@main def hello(): Unit =
  println("Hello world!")
  println(msg)
  println(Foo.all)

def msg = "I was compiled by Scala 3. :)"
