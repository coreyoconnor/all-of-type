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

object Zob {
  val x: String = "lkee"
}

trait T0
object Zic extends T0
object Zid extends T0

@AllOfType
class Zob {
  val x: T0 = Zic
  val y = Zid

  val all: Seq[T0] = AllOfType.derive
}

@main def hello(): Unit =
  println("Hello world!")
  println(msg)
  println(Foo.all)
  println(Foo.other)
  println(Zib.count)
  println((new Zob).all)
  println(Zob.x)

def msg = "I was compiled by Scala 3. :)"
