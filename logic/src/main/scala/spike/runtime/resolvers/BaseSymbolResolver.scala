package spike.runtime.resolvers

import cats.Applicative
import cats.implicits._
import io.circe.Json
import spike.BaseSymbols.Predicate._
import spike.BaseSymbols._

object BaseSymbolResolver {

  def convertToBaseSymbol[F[_]: Applicative, A <: spike.CommonSymbols](s: A)(symbol: s.Symbol)(convert: s.OwnSymbols => F[Json]): F[Symbol] = {
    val convertSym  = convertToBaseSymbol(s)(_: s.Symbol)(convert)
    val convertPred = convertToBasePredicate(s)(_: s.Predicate)(convert)
    symbol match {
      case x: s.Literal               => (Literal(x.value): Symbol).pure[F]
      case x: s.LambdaParameter       => (LambdaParameter(x.distance): Symbol).pure[F]
      case x: s.Map                   => (convertSym(x.symbol), convertSym(x.path)).mapN(Map.apply)
      case x: s.FlatMap               => (convertSym(x.symbol), convertSym(x.path)).mapN(FlatMap.apply)
      case x: s.Flatten               => convertSym(x.symbol).map(Flatten.apply)
      case x: s.Find                  => (convertSym(x.symbol), convertPred(x.predicate)).mapN(Find.apply)
      case x: s.Count                 => convertSym(x.symbol).map(Count.apply)
      case x: s.Distinct              => convertSym(x.symbol).map(Distinct.apply)
      case x: s.Prepend               => (convertSym(x.item), convertSym(x.collection)).mapN(Prepend.apply)
      case x: s.Append                => (convertSym(x.collection), convertSym(x.item)).mapN(Append.apply)
      case x: s.Concat                => (convertSym(x.leftCollection), convertSym(x.rightCollection)).mapN(Concat.apply)
      case x: s.Predicate             => convertPred(x).widen[Symbol]
      case x: s.OwnSymbols @unchecked => convert(x).map(Literal.apply)
      case x                          => throw new Exception(s"No matches for $x in case statement.")
    }
  }

  def convertToBasePredicate[F[_]: Applicative, A <: spike.CommonSymbols](s: A)(symbol: s.Predicate)(convert: s.OwnSymbols => F[Json]): F[Predicate] = {
    val convertSym  = convertToBaseSymbol(s)(_: s.Symbol)(convert)
    val convertPred = convertToBasePredicate(s)(_: s.Predicate)(convert)
    symbol match {
      case x: s.Predicate.Equals   => (convertSym(x.left), convertSym(x.right)).mapN(Equals.apply)
      case x: s.Predicate.And      => (convertPred(x.left), convertPred(x.right)).mapN(And.apply)
      case x: s.Predicate.Or       => (convertPred(x.left), convertPred(x.right)).mapN(Or.apply)
      case x: s.Predicate.Not      => convertPred(x.predicate).map(Not.apply)
      case x: s.Predicate.Exists   => (convertSym(x.symbol), convertPred(x.predicate)).mapN(Exists.apply)
      case x: s.Predicate.Contains => (convertSym(x.collection), convertSym(x.item)).mapN(Contains.apply)
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
