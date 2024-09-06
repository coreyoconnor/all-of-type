import scala.languageFeature.experimental

object Zib {
  var count = 0
}

class Zib {
  Zib.count += 1
}

@AllOfType
object Foo {
  val x: String = "ASDAS"
  val y = "ASDAS"

  val all: Seq[String] = AllOfType.derive

  val za = new Zib
  val zb: Zib = new Zib

  val other: Seq[Zib] = AllOfType.derive
}

@main def hello(): Unit =
  println("Hello world!")
  println(msg)
  println(Foo.all)
  println(Foo.other)
  println(Zib.count)

def msg = "I was compiled by Scala 3. :)"
