package spike

import io.circe.Json
import spike.schema.EndpointId

sealed trait CommonSymbols {
  sealed trait Symbol

  case class Literal(value: Json) extends Symbol

  case class LambdaParameter(distance: Int) extends Symbol // 0 is current lambda's param, 1 is parent, 2 is grandparent, etc. Used with things like 'Exists'

  // Derivatives
  case class Map(symbol: Symbol, path: List[String]) extends Symbol
  case class Flatten(symbol: Symbol) extends Symbol
  case class FindOne(symbol: Symbol, predicate: Predicate) extends Symbol
  case class Count(symbol: Symbol) extends Symbol
  case class Distinct(symbol: Symbol) extends Symbol

  def FlatMap(symbol: Symbol, path: List[String]): Symbol =
    Flatten(Map(symbol, path))

  sealed trait Predicate extends Symbol
  object Predicate {
    case class Equals(left: Symbol, right: Symbol) extends Predicate
    case class And(left: Predicate, right: Predicate) extends Predicate
    case class Or(left: Predicate, right: Predicate) extends Predicate
    case class Not(predicate: Predicate) extends Predicate

    def Exists(symbol: Symbol, predicate: Predicate): Predicate =
      Equals(Count(FindOne(symbol, predicate)), Literal(Json.fromInt(1)))

    def Contains(collection: Symbol, item: Symbol): Predicate =
      Exists(collection, Equals(item, LambdaParameter(0)))
  }
}

object SchemaSymbols extends CommonSymbols {
  case class Parameter(name: String) extends Symbol
  case object Result extends Symbol
  case class Endpoint(endpointId: EndpointId, parameters: scala.collection.Map[String, Symbol], evaluateAfterExecution: Boolean) extends Symbol // 'stateAfterExecution' only valid for post-conditions (is this shows the current endpoint is modifying state read by this endpoint, so we should check that endpoint is healthy too)
}

object RuntimeSymbols extends CommonSymbols {
  case class Result(precedingRequestDistance: Int) extends Symbol
}