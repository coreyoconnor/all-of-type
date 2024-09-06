import scala.quoted.*

import scala.annotation.StaticAnnotation
import scala.annotation.MacroAnnotation
import scala.annotation.experimental

object AllOfType {
  val derive: Nothing =
    throw new NotImplementedError("Apply the @AllOfType annotation to containing class/object")
}

/** EG:
  * ```scala
  * @AllOfType
  * object Foo {
  *   val x: String = "ASDAS"
  *   val y = "ASDAS"
  *   val all: Seq[String] = AllOfType.derive
  * }
  * ```
  *
  * Will result in the `all` value equal to `Seq(x, y)`.
  */
@experimental
class AllOfType extends MacroAnnotation {

  def transform(using quotes: Quotes)(
    definition: quotes.reflect.Definition,
    companion: Option[quotes.reflect.Definition]
  ): List[quotes.reflect.Definition] = {
    import quotes.reflect.*

    def updateBody(body: List[Statement]): List[Statement] = {
      val allSeqs = collectAllSeqsToBuild(body)

      val mapToTargets = buildMapToTargets(body, allSeqs)

      body map {
        case maybeAllSeq: ValDef => mapToTargets.get(maybeAllSeq.symbol) match {
          case None => maybeAllSeq
          case Some(targetVals) => replacementImpl(maybeAllSeq, targetVals)
        }
        case stmt => stmt
      }
    }

    val updatedDef = definition match {
      case ClassDef(name, constructor, parents, self, body) =>
        ClassDef.copy(definition)(name, constructor, parents, self, updateBody(body))
    }

    List(Some(updatedDef), companion).flatten
  }

/**
  * Replacement implementation for a derived "all" sequence.
  *
  * @param quotes
  * @param allSeq
  * @param targetVals
  * @return
  */
  private def replacementImpl(using quotes: Quotes)(
    allSeq: quotes.reflect.ValDef,
    targetVals: List[quotes.reflect.Symbol]
  ): quotes.reflect.ValDef = {
    import quotes.reflect.*

    val targetRefArgs = Varargs(targetVals.map(Ref(_).asExpr))
    val replacement = '{Seq($targetRefArgs)}.asTerm
    ValDef.copy(allSeq)(allSeq.name, allSeq.tpt, Some(replacement))
  }

/**
  * Map from an "all" sequence symbol to the type of `val`s that should be included.
  *
  * @param quotes
  * @param body
  * @return
  */
  private def collectAllSeqsToBuild(using quotes: Quotes)(
    body: List[quotes.reflect.Statement]
  ): Map[quotes.reflect.Symbol, Type[?]] = {
    import quotes.reflect.*

    body.flatMap {
      case valDef@ValDef(_, valType, Some(valImpl)) =>
        (valType.tpe.asType -> valImpl.asExpr) match {
          case ('[Seq[innerType]], '{AllOfType.derive}) => Some(valDef.symbol -> Type.of[innerType])
          case _ => None
        }
      case _ => None
    }.toMap
  }

/**
  * Map from an "all" `Seq` to the `val`s to include.
  *
  * @param quotes
  * @param body
  * @param allSeqs
  * @return
  */
  private def buildMapToTargets(using quotes: Quotes)(
    body: List[quotes.reflect.Statement],
    allSeqs: Map[quotes.reflect.Symbol, Type[?]]
  ): Map[quotes.reflect.Symbol, List[quotes.reflect.Symbol]] = {
    import quotes.reflect.*

    def ignore = allSeqs.keySet

    body.collect {
        case valDef: ValDef if !ignore.contains(valDef.symbol) => valDef
    }.flatMap { possibleTarget =>
      val valType = possibleTarget.tpt.tpe.asType

      allSeqs.collect {
        case (allSeq, targetType)
          if TypeRepr.of(using targetType) <:< TypeRepr.of(using valType)
            => allSeq -> possibleTarget.symbol
      }
    }.groupMap(_._1)(_._2)
  }
}

