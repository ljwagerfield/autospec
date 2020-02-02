package spike.runtime

import spike.RuntimeSymbols.Result
import cats.implicits._
import io.circe.Json
import spike.RuntimeSymbols._

object SymbolResolver {
  def resolveSymbol(history: List[EndpointRequestResponse], symbol: Symbol): Json =
    resolveSymbol(history, Nil, symbol)

  def resolvePredicate(history: List[EndpointRequestResponse], predicate: Predicate): Boolean =
    resolvePredicate(history, Nil, predicate)

  private def resolvePredicate(history: List[EndpointRequestResponse], lambdaParameterStack: List[Json], predicate: Predicate): Boolean = {
    val resolveSym  = resolveSymbol(history, lambdaParameterStack, _: Symbol)
    val resolvePred = resolvePredicate(history, lambdaParameterStack, _: Predicate)
    predicate match {
      case Predicate.Equals(left, right) => resolveSym(left) === resolveSym(right)
      case Predicate.And(left, right)    => resolvePred(left) && resolvePred(right)
      case Predicate.Or(left, right)     => resolvePred(left) || resolvePred(right)
      case Predicate.Not(pred)           => !resolvePred(pred)
    }
  }

  private def resolveSymbol(history: List[EndpointRequestResponse], lambdaParameterStack: List[Json], symbol: Symbol): Json = {
    val resolve = resolveSymbol(history, lambdaParameterStack, _: Symbol)
    symbol match {
      case Literal(value)                   => value
      case LambdaParameter(distance)        => lambdaParameterStack(distance)
      case Result(precedingRequestDistance) => history(precedingRequestDistance).response.body
      case Map(symbol, path)                => path.foldLeft(resolve(symbol))(mapJson)
      case Flatten(symbol)                  => flatten(resolve(symbol))
      case FindOne(symbol, predicate)       =>
        toVector(resolveSymbol(history, lambdaParameterStack, symbol))
          .find(json => resolvePredicate(history, json :: lambdaParameterStack, predicate))
          .getOrElse(Json.Null)
      case Count(symbol)                    => Json.fromInt(toVector(resolve(symbol)).size)
      case Distinct(symbol)                 => Json.fromValues(toVector(resolve(symbol)).distinct)
      case predicate: Predicate             => Json.fromBoolean(resolvePredicate(history, lambdaParameterStack, predicate))
    }
  }

  // We use 'Json.Null' instead of 'None: Option[Json]' since optionality may occur _within_ JSON structures too, and
  // we can only use 'Json.Null' there. E.g. 'Map' operations cannot change the size of an array, and since objects in
  // arrays may be different shapes, 'Map' operations may need to yield 'Json.Null' for some elements but not others.
  private def mapJson(json: Json, key: String): Json =
    json.fold(
      Json.Null,
      _ => Json.Null,
      _ => Json.Null,
      _ => Json.Null,
      x => Json.fromValues(x.map(mapJson(_, key))),
      x => x(key).getOrElse(Json.Null)
    )

  private def flatten(json: Json): Json =
    json
      .asArray
      .map(x => Json.fromValues(x.flatMap(toVector)))
      .getOrElse(json)

  private def toVector(json: Json): Vector[Json] =
    json.asArray.getOrElse(if (json.isNull) Vector.empty else Vector(json))
}
