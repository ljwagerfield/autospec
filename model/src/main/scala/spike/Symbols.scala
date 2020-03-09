package spike

import io.circe.Json
import spike.schema.{EndpointId, EndpointParameterName}

sealed trait CommonSymbols {
  sealed trait Symbol

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

object SchemaSymbols extends CommonSymbols {
  case class Parameter(name: EndpointParameterName) extends Symbol
  case object ResponseBody extends Symbol
  case object StatusCode extends Symbol
  case class Endpoint(endpointId: EndpointId, parameters: scala.collection.immutable.Map[EndpointParameterName, Symbol], evaluateAfterExecution: Boolean) extends Symbol // 'stateAfterExecution' should be set to 'false' for preconditions. If set to 'true' it will never be checked.
}

object ResolvedSymbols extends CommonSymbols {
  case class Parameter(name: EndpointParameterName) extends Symbol
}

object RuntimeSymbols extends CommonSymbols {
  case class ResponseBody(requestIndex: Int) extends Symbol
  case class StatusCode(requestIndex: Int) extends Symbol
}