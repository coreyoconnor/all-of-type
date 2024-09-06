import scala.quoted.*

import scala.annotation.StaticAnnotation
import scala.annotation.MacroAnnotation
import scala.annotation.experimental

object AllOfType {
  val derive: Nothing =
    throw new UninitializedFieldError("Apply the @AllOfType annotation to containing class/object")
}

@experimental
class AllOfType extends MacroAnnotation {

  def transform(using quotes: Quotes)(
    definition: quotes.reflect.Definition,
    companion: Option[quotes.reflect.Definition]
  ): List[quotes.reflect.Definition] = {
    import quotes.reflect.*

    sealed trait StmtAction
    case class Pass(stmt: Statement) extends StmtAction
    case class SeqToBuild(valDef: ValDef, innerType: Type[?]) extends StmtAction
    case class ValToInclude(valDef: ValDef, valType: Type[?]) extends StmtAction

    def updateBody(body: List[Statement]): List[Statement] = {

      val actions = body map {
        case valDef@ValDef(_, valType, Some(valImpl)) =>
          valType.tpe.asType match {
            case '[Seq[innerType]] => {
              valImpl.asExpr match {
                case '{AllOfType.derive} => SeqToBuild(valDef, Type.of[innerType])
                case stmt => Pass(valDef)
              }
            }
            case tpe => ValToInclude(valDef, tpe)
          }
        case stmt => Pass(stmt)
      }

      val allSeqs = actions.collect { case SeqToBuild(seqDef, innerType) =>
        val targetSymbols = actions.collect {
          case ValToInclude(valDef, targetType)
            if TypeRepr.of(using targetType) <:< TypeRepr.of(using innerType) => valDef.symbol
        }

        val targetRefs = Varargs(targetSymbols.map(Ref(_).asExpr))
        val targetRefSeqs = '{ Seq($targetRefs) }.asTerm

        seqDef.symbol -> ValDef.copy(seqDef)(seqDef.name, seqDef.tpt, Some(targetRefSeqs))
      }.toMap

      body map { stmt =>
        allSeqs.get(stmt.symbol) match {
          case Some(replacement) => replacement
          case None => stmt
        }
      }
    }

    val updatedDef = definition match {
      case ClassDef(name, constructor, parents, self, body) =>
        ClassDef.copy(definition)(name, constructor, parents, self, updateBody(body))
    }

    List(Some(updatedDef), companion).flatten
  }
}

