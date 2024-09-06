import scala.quoted.*

import scala.annotation.StaticAnnotation
import scala.annotation.MacroAnnotation
import scala.annotation.experimental
import scala.annotation.targetName
import scala.annotation.varargs

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
    case class Passthrough(stmt: Statement) extends StmtAction
    case class AllSeq(valDef: ValDef, targetType: Type[?]) extends StmtAction
    case class AllSeqTarget(valDef: ValDef, tpe: Type[?]) extends StmtAction

    val updatedDef = definition match {
      case ClassDef(name, constructor, parents, self, body) =>
        val targetType = TypeRepr.of[String].typeSymbol

        val allOfType = body collect {
          case valDef@ValDef(_, valType, Some(valImpl)) =>
            valType.tpe.asType match {
              case '[Seq[targetType]] => {
                valImpl.asExpr match {
                  case '{AllOfType.derive} => AllSeq(valDef, Type.of[targetType])
                  case stmt => Passthrough(valDef)
                }
              }
              case tpe => AllSeqTarget(valDef, tpe)
            }
          case stmt => Passthrough(stmt)
        }

        val onlyValSeqs = allOfType collect {
          case AllSeq(valDef, targetType) => valDef -> targetType
        }

        val collectedDefs = onlyValSeqs.map { case (allSeqDef, allSeqType) =>
          allSeqDef.symbol -> (
            allSeqDef,
            allOfType.collect {
              case AllSeqTarget(valDef, targetType)
                if TypeRepr.of(using targetType) <:< TypeRepr.of(using allSeqType) =>
                  valDef.symbol
            }
          )
        }.toMap

        val updatedBody = body map { stmt =>
          collectedDefs.get(stmt.symbol) match {
            case Some((allSeqDef, names)) => {
              val namesRefArgs = Varargs(names.map(Ref(_).asExpr))
              val newRhs = '{ Seq($namesRefArgs) }.asTerm

              ValDef.copy(allSeqDef)(allSeqDef.name, allSeqDef.tpt, Some(newRhs))
            }
            case None => stmt
          }
        }

        ClassDef.copy(definition)(name, constructor, parents, self, updatedBody)
    }

    List(Some(updatedDef), companion).flatten
  }
}

