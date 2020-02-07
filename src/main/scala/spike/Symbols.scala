package spike

import cats.data.NonEmptyList
import io.circe.Json
import spike.schema.{EndpointId, EndpointParameterName}

sealed trait CommonSymbols {
  sealed trait Symbol

  case class Literal(value: Json) extends Symbol

  case class LambdaParameter(distance: Int) extends Symbol // 0 is current lambda's param, 1 is parent, 2 is grandparent, etc. Used with things like 'Exists'

  // Derivatives
  case class Map(symbol: Symbol, path: NonEmptyList[String]) extends Symbol
  case class Flatten(symbol: Symbol) extends Symbol
  case class Find(symbol: Symbol, predicate: Predicate) extends Symbol
  case class Count(symbol: Symbol) extends Symbol
  case class Distinct(symbol: Symbol) extends Symbol

  def FlatMap(symbol: Symbol, path: NonEmptyList[String]): Symbol =
    Flatten(Map(symbol, path))

  sealed trait Predicate extends Symbol
  object Predicate {
    case class Equals(left: Symbol, right: Symbol) extends Predicate
    case class And(left: Predicate, right: Predicate) extends Predicate
    case class Or(left: Predicate, right: Predicate) extends Predicate
    case class Not(predicate: Predicate) extends Predicate

    def Exists(symbol: Symbol, predicate: Predicate): Predicate =
      Equals(Count(Find(symbol, predicate)), Literal(Json.fromInt(1)))

    def Contains(collection: Symbol, item: Symbol): Predicate =
      Exists(collection, Equals(LambdaParameter(0), item))
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
  case class Endpoint(endpointId: EndpointId, parameters: scala.collection.immutable.Map[EndpointParameterName, Symbol], evaluateAfterExecution: Boolean) extends Symbol // 'stateAfterExecution' always false for pre-conditions.
}

object RuntimeSymbols extends CommonSymbols {
  case class ResponseBody(requestIndex: Int) extends Symbol
  case class StatusCode(requestIndex: Int) extends Symbol
}