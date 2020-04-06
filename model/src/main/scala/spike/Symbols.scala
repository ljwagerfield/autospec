package spike

import io.circe.Json
import spike.schema.{EndpointId, EndpointParameterName}

sealed trait CommonSymbols {
  type OwnSymbols <: Symbol

  sealed trait Symbol extends Product with Serializable

  // Leafs
  case class Literal(value: Json) extends Symbol
  case class LambdaParameter(distance: Int) extends Symbol // 0 is current lambda's param, 1 is parent, 2 is grandparent, etc. Used with things like 'Exists'

  // Recursive Symbols
  case class Map(symbol: Symbol, path: Symbol) extends Symbol
  case class Flatten(symbol: Symbol) extends Symbol
  case class FlatMap(symbol: Symbol, path: Symbol) extends Symbol
  case class Find(symbol: Symbol, predicate: Predicate) extends Symbol
  case class Count(symbol: Symbol) extends Symbol
  case class Distinct(symbol: Symbol) extends Symbol
  case class Prepend(item: Symbol, collection: Symbol) extends Symbol
  case class Append(collection: Symbol, item: Symbol) extends Symbol
  case class Concat(leftCollection: Symbol, rightCollection: Symbol) extends Symbol

  sealed trait Predicate extends Symbol
  object Predicate {
    case class Equals(left: Symbol, right: Symbol) extends Predicate
    case class And(left: Predicate, right: Predicate) extends Predicate
    case class Or(left: Predicate, right: Predicate) extends Predicate
    case class Not(predicate: Predicate) extends Predicate
    case class Exists(symbol: Symbol, predicate: Predicate) extends Predicate
    case class Contains(collection: Symbol, item: Symbol) extends Predicate
  }

  object Literal {
    def apply(value: Int): Literal =
      Literal(Json.fromInt(value))
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
  case object ResponseBody extends SchemaSymbol
  case object StatusCode extends SchemaSymbol
  case class Endpoint(endpointId: EndpointId, parameters: scala.collection.immutable.Map[EndpointParameterName, Symbol], evaluateAfterExecution: Boolean) extends SchemaSymbol // 'stateAfterExecution' should be set to 'false' for preconditions. If set to 'true' it will never be checked.
}

object IntermediateSymbols extends CommonSymbols {
  type OwnSymbols = ResolvedPreconditionSymbol

  sealed trait ResolvedPreconditionSymbol extends Symbol

  case class Parameter(name: EndpointParameterName) extends ResolvedPreconditionSymbol
}

object RuntimeSymbols extends CommonSymbols {
  type OwnSymbols = RuntimeSymbol

  sealed trait RuntimeSymbol extends Symbol

  case class ResponseBody(requestIndex: Int) extends RuntimeSymbol
  case class StatusCode(requestIndex: Int) extends RuntimeSymbol
}