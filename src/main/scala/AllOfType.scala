import scala.quoted.*

import scala.annotation.StaticAnnotation
import scala.annotation.MacroAnnotation
import scala.annotation.experimental

object AllOfType {
  val derive: Nothing = ???
}

@experimental
class AllOfType extends MacroAnnotation {
  def transform(using quotes: Quotes)(
    definition: quotes.reflect.Definition,
    companion: Option[quotes.reflect.Definition]
  ): List[quotes.reflect.Definition] = {
    import quotes.reflect.*

    println(definition)

    definition match {
      case ClassDef(_, _, _, _, tmpl) => {
        println(tmpl)
        ???
      }
      case _ => ???
    }
  }
}

