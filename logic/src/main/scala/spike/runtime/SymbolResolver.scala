package spike.runtime

import spike.RuntimeSymbols.ResponseBody
import cats.implicits._
import io.circe.Json
import spike.RuntimeSymbols._
import spike.RuntimeSymbols.Predicate._

object SymbolResolver {
  def resolveSymbol(history: List[EndpointRequestResponse], symbol: Symbol): Json =
    resolveSymbol(history, Nil, symbol)

  def resolvePredicate(history: List[EndpointRequestResponse], predicate: Predicate): Boolean =
    resolvePredicate(history, Nil, predicate)

  private def resolvePredicate(history: List[EndpointRequestResponse], lambdaParameterStack: List[Json], predicate: Predicate): Boolean = {
    val resolveSym  = resolveSymbol(history, lambdaParameterStack, _: Symbol)
    val resolvePred = resolvePredicate(history, lambdaParameterStack, _: Predicate)
    predicate match {
      case Equals(left, right)        => resolveSym(left) === resolveSym(right)
      case And(left, right)           => resolvePred(left) && resolvePred(right)
      case Or(left, right)            => resolvePred(left) || resolvePred(right)
      case Not(pred)                  => !resolvePred(pred)
      case Exists(symbol, pred)       => toVector(resolveSym(symbol)).exists(json => resolvePredicate(history, json :: lambdaParameterStack, pred))
      case Contains(collection, item) => toVector(resolveSym(collection)).contains_(resolveSym(item))
    }
  }

  private def resolveSymbol(history: List[EndpointRequestResponse], lambdaParameterStack: List[Json], symbol: Symbol): Json = {
    val resolve    = resolveSymbol(history, lambdaParameterStack, _: Symbol)
    val responseAt = (requestIndex: Int) => history(history.size - (requestIndex + 1)).response
    symbol match {
      case Literal(value)                   => value
      case LambdaParameter(distance)        => lambdaParameterStack(distance)
      case ResponseBody(requestIndex)       => responseAt(requestIndex).body
      case StatusCode(requestIndex)         => Json.fromInt(responseAt(requestIndex).status)
      case Map(symbol, path)                => toVector(resolve(path)).foldLeft(resolve(symbol))(mapJson)
      case FlatMap(symbol, path)            => resolve(Flatten(Map(symbol, path)))
      case Flatten(symbol)                  => flatten(resolve(symbol))
      case Find(symbol, predicate)          =>
        toVector(resolve(symbol))
          .find(json => resolvePredicate(history, json :: lambdaParameterStack, predicate))
          .getOrElse(Json.Null)
      case Count(symbol)                    => Json.fromInt(toVector(resolve(symbol)).size)
      case Distinct(symbol)                 => Json.fromValues(toVector(resolve(symbol)).distinct)
      case Prepend(item, collection)        => Json.fromValues(resolve(item) +: toVector(resolve(collection)))
      case Append(collection, item)         => Json.fromValues(toVector(resolve(collection)) :+ resolve(item))
      case Concat(left, right)              => Json.fromValues(toVector(resolve(left)) ++ toVector(resolve(right)))
      case predicate: Predicate             => Json.fromBoolean(resolvePredicate(history, lambdaParameterStack, predicate))
    }
  }

  // We use 'Json.Null' instead of 'None: Option[Json]' since optionality may occur _within_ JSON structures too, and
  // we can only use 'Json.Null' there. E.g. 'Map' operations cannot change the size of an array, and since objects in
  // arrays may be different shapes, 'Map' operations may need to yield 'Json.Null' for some elements but not others.
  private def mapJson(json: Json, key: Json): Json =
    json.fold(
      Json.Null,
      _ => Json.Null,
      _ => Json.Null,
      _ => Json.Null,
      x => Json.fromValues(x.map(mapJson(_, key))),
      x => x(key.toString()).getOrElse(Json.Null) // Todo: check if we can access array elements this way.
    )

  private def flatten(json: Json): Json =
    json
      .asArray
      .map(x => Json.fromValues(x.flatMap(toVector)))
      .getOrElse(json)

  private def toVector(json: Json): Vector[Json] =
    json.asArray.getOrElse(if (json.isNull) Vector.empty else Vector(json))
}
