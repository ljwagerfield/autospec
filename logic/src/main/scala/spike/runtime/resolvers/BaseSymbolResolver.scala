package spike.runtime.resolvers

import cats.implicits._
import io.circe.Json
import spike.BaseSymbols.Predicate._
import spike.BaseSymbols._

object BaseSymbolResolver {
  def convertToBaseSymbol[A <: spike.CommonSymbols](s: A)(symbol: s.Symbol)(convert: s.OwnSymbols => Json): Symbol = {
    val convertSym  = convertToBaseSymbol(s)(_: s.Symbol)(convert)
    val convertPred = convertToBasePredicate(s)(_: s.Predicate)(convert)
    symbol match {
      case x: s.Literal               => Literal(x.value)
      case x: s.LambdaParameter       => LambdaParameter(x.distance)
      case x: s.Map                   => Map(convertSym(x.symbol), convertSym(x.path))
      case x: s.FlatMap               => FlatMap(convertSym(x.symbol), convertSym(x.path))
      case x: s.Flatten               => Flatten(convertSym(x.symbol))
      case x: s.Find                  => Find(convertSym(x.symbol), convertPred(x.predicate))
      case x: s.Count                 => Count(convertSym(x.symbol))
      case x: s.Distinct              => Distinct(convertSym(x.symbol))
      case x: s.Prepend               => Prepend(convertSym(x.item), convertSym(x.collection))
      case x: s.Append                => Append(convertSym(x.collection), convertSym(x.item))
      case x: s.Concat                => Concat(convertSym(x.leftCollection), convertSym(x.rightCollection))
      case x: s.Predicate             => convertPred(x)
      case x: s.OwnSymbols @unchecked => Literal(convert(x))
      case x                          => throw new Exception(s"No matches for $x in case statement.")
    }
  }

  def convertToBasePredicate[A <: spike.CommonSymbols](s: A)(symbol: s.Predicate)(convert: s.OwnSymbols => Json): Predicate = {
    val convertSym  = convertToBaseSymbol(s)(_: s.Symbol)(convert)
    val convertPred = convertToBasePredicate(s)(_: s.Predicate)(convert)
    symbol match {
      case x: s.Predicate.Equals   => Equals(convertSym(x.left), convertSym(x.right))
      case x: s.Predicate.And      => And(convertPred(x.left), convertPred(x.right))
      case x: s.Predicate.Or       => Or(convertPred(x.left), convertPred(x.right))
      case x: s.Predicate.Not      => Not(convertPred(x.predicate))
      case x: s.Predicate.Exists   => Exists(convertSym(x.symbol), convertPred(x.predicate))
      case x: s.Predicate.Contains => Contains(convertSym(x.collection), convertSym(x.item))
      case x                       => throw new Exception(s"No matches for $x in case statement.")
    }
  }

  def resolveSymbol(symbol: Symbol): Json =
    resolveSymbol(Nil, symbol)

  def resolvePredicate(predicate: Predicate): Boolean =
    resolvePredicate(Nil, predicate)

  private def resolvePredicate(lambdaParameterStack: List[Json], predicate: Predicate): Boolean = {
    val resolveSym  = resolveSymbol(lambdaParameterStack, _: Symbol)
    val resolvePred = resolvePredicate(lambdaParameterStack, _: Predicate)
    predicate match {
      case Equals(left, right)        => resolveSym(left) === resolveSym(right)
      case And(left, right)           => resolvePred(left) && resolvePred(right)
      case Or(left, right)            => resolvePred(left) || resolvePred(right)
      case Not(pred)                  => !resolvePred(pred)
      case Exists(symbol, pred)       => toVector(resolveSym(symbol)).exists(json => resolvePredicate(json :: lambdaParameterStack, pred))
      case Contains(collection, item) => toVector(resolveSym(collection)).contains_(resolveSym(item))
    }
  }

  private def resolveSymbol(lambdaParameterStack: List[Json], symbol: Symbol): Json = {
    val resolve = resolveSymbol(lambdaParameterStack, _: Symbol)
    symbol match {
      case Literal(value)                   => value
      case LambdaParameter(distance)        => lambdaParameterStack(distance)
      case Map(symbol, path)                => toVector(resolve(path)).foldLeft(resolve(symbol))(mapJson)
      case FlatMap(symbol, path)            => resolve(Flatten(Map(symbol, path)))
      case Flatten(symbol)                  => flatten(resolve(symbol))
      case Find(symbol, predicate)          =>
        toVector(resolve(symbol))
          .find(json => resolvePredicate(json :: lambdaParameterStack, predicate))
          .getOrElse(Json.Null)
      case Count(symbol)                    => Json.fromInt(toVector(resolve(symbol)).size)
      case Distinct(symbol)                 => Json.fromValues(toVector(resolve(symbol)).distinct)
      case Prepend(item, collection)        => Json.fromValues(resolve(item) +: toVector(resolve(collection)))
      case Append(collection, item)         => Json.fromValues(toVector(resolve(collection)) :+ resolve(item))
      case Concat(left, right)              => Json.fromValues(toVector(resolve(left)) ++ toVector(resolve(right)))
      case predicate: Predicate             => Json.fromBoolean(resolvePredicate(lambdaParameterStack, predicate))
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
