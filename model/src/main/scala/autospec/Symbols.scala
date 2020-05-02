package autospec

import autospec.runtime.{EndpointRequestId, EndpointRequestIndex}
import io.circe.Json
import autospec.schema.{EndpointId, EndpointParameterName}
import scala.collection.immutable.{Map => SMap}

sealed trait CommonSymbols {
  type OwnSymbols <: Symbol

  sealed trait Symbol extends Product with Serializable

  // Leafs
  case class Literal(value: Json) extends Symbol

  case class LambdaParameter(distance: Int)
    extends Symbol // 0 is current lambda's param, 1 is parent, 2 is grandparent, etc. Used with things like 'Exists'

  // Recursive Symbols
  case class ValueAt(symbol: Symbol, key: Symbol)                    extends Symbol // If key is an array, will be traversed.
  case class Map(symbol: Symbol, function: Symbol)                   extends Symbol
  case class Flatten(symbol: Symbol)                                 extends Symbol
  case class FlatMap(symbol: Symbol, function: Symbol)               extends Symbol
  case class Find(symbol: Symbol, predicate: Predicate)              extends Symbol
  case class Count(symbol: Symbol)                                   extends Symbol
  case class Add(left: Symbol, right: Symbol)                        extends Symbol
  case class Subtract(left: Symbol, right: Symbol)                   extends Symbol
  case class Multiply(left: Symbol, right: Symbol)                   extends Symbol
  case class Divide(left: Symbol, right: Symbol)                     extends Symbol
  case class Distinct(symbol: Symbol)                                extends Symbol
  case class Concat(leftCollection: Symbol, rightCollection: Symbol) extends Symbol // To add an element, use 'Add'

  sealed trait Predicate extends Symbol

  object Predicate {
    case class Equals(left: Symbol, right: Symbol)          extends Predicate
    case class And(left: Predicate, right: Predicate)       extends Predicate
    case class Or(left: Predicate, right: Predicate)        extends Predicate
    case class Not(predicate: Predicate)                    extends Predicate
    case class Exists(symbol: Symbol, predicate: Predicate) extends Predicate
    case class Contains(collection: Symbol, item: Symbol)   extends Predicate
  }

  object Literal {

    def apply(value: Int): Literal =
      Literal(Json.fromInt(value))

    def apply(value: String): Literal =
      Literal(Json.fromString(value))

  }

}

object BaseSymbols extends CommonSymbols {
  // ------------------------------------
  // Must not contain any custom symbols!
  // ------------------------------------
  // The purpose of 'BaseSymbols' is to provide a concrete common type the other symbol families can be converted to,
  // such that common code can be safely written that works against all families (requires users to resolve custom
  // types to literals first, though).
  // ------------------------------------
}

object SchemaSymbols extends CommonSymbols {
  type OwnSymbols = SchemaSymbol

  sealed trait SchemaSymbol extends Symbol

  case class Parameter(name: EndpointParameterName) extends SchemaSymbol
  case object ResponseBody                          extends SchemaSymbol
  case object StatusCode                            extends SchemaSymbol

  // 'evaluateAfterExecution' should be set to 'false' for preconditions. If set to 'true' it will never be checked.
  case class Endpoint(
    endpointId: EndpointId,
    parameters: SMap[EndpointParameterName, Symbol],
    evaluateAfterExecution: Boolean
  ) extends SchemaSymbol

  object Parameter {

    def apply(name: String): Parameter =
      Parameter(EndpointParameterName(name))

  }

}

object IntermediateSymbols extends CommonSymbols {
  type OwnSymbols = ResolvedPreconditionSymbol

  sealed trait ResolvedPreconditionSymbol extends Symbol

  case class Parameter(name: EndpointParameterName) extends ResolvedPreconditionSymbol
}

sealed trait RuntimeSymbolsLike[A] extends CommonSymbols {
  type OwnSymbols = RuntimeSymbol

  sealed trait RuntimeSymbol extends Symbol

  case class ResponseBody(requestRef: A) extends RuntimeSymbol
  case class StatusCode(requestRef: A)   extends RuntimeSymbol
}

object RuntimeSymbolsIndexed extends RuntimeSymbolsLike[EndpointRequestIndex]

object RuntimeSymbolsExecuted extends RuntimeSymbolsLike[EndpointRequestId]
