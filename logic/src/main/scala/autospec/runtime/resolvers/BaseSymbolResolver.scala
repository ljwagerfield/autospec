package autospec.runtime.resolvers

import cats.Applicative
import cats.implicits._
import io.circe.Json
import autospec.common.JsonExtensions._
import autospec.BaseSymbols
import autospec.BaseSymbols.Predicate._
import autospec.BaseSymbols._

object BaseSymbolResolver {

  def convertToBaseSymbol[F[_]: Applicative, A <: autospec.CommonSymbols](
    family: A
  )(symbol: family.Symbol)(convert: family.OwnSymbols => F[Json]): F[Symbol] =
    SymbolConverter.convertSymbol(family, BaseSymbols)(symbol)(symbol => convert(symbol).map(Literal.apply))

  def convertToBasePredicate[F[_]: Applicative, A <: autospec.CommonSymbols](
    family: A
  )(symbol: family.Predicate)(convert: family.OwnSymbols => F[Json]): F[Predicate] =
    SymbolConverter.convertPredicate(family, BaseSymbols)(symbol)(symbol => convert(symbol).map(Literal.apply))

  def resolveSymbol(symbol: Symbol): Json =
    resolveSymbol(Nil, symbol)

  def resolvePredicate(predicate: Predicate): Boolean =
    resolvePredicate(Nil, predicate)

  private def resolvePredicate(lambdaParameterStack: List[Json], predicate: Predicate): Boolean = {
    val resolveSym  = resolveSymbol(lambdaParameterStack, _: Symbol)
    val resolvePred = resolvePredicate(lambdaParameterStack, _: Predicate)
    predicate match {
      case Equals(left, right) => resolveSym(left) === resolveSym(right)
      case And(left, right)    => resolvePred(left) && resolvePred(right)
      case Or(left, right)     => resolvePred(left) || resolvePred(right)
      case Not(pred)           => !resolvePred(pred)
      case Exists(symbol, pred) =>
        toVector(resolveSym(symbol)).exists(json => resolvePredicate(json :: lambdaParameterStack, pred))
      case Contains(collection, item) => toVector(resolveSym(collection)).contains_(resolveSym(item))
    }
  }

  private def resolveSymbol(lambdaParameterStack: List[Json], symbol: Symbol): Json = {
    val resolve     = resolveSymbol(lambdaParameterStack, _: Symbol)
    val resolvePred = resolvePredicate(lambdaParameterStack, _: Predicate)
    symbol match {
      case Literal(value)            => value
      case LambdaParameter(distance) => lambdaParameterStack(distance)
      case ValueAt(symbol, path)     => toVector(resolve(path)).foldLeft(resolve(symbol))(valueAt)
      case Map(symbol, function) =>
        Json.fromValues(
          toVector(resolve(symbol))
            .map(json => resolveSymbol(json :: lambdaParameterStack, function))
        )
      case FlatMap(symbol, function)        => resolve(Flatten(Map(symbol, function)))
      case Flatten(symbol)                  => flatten(resolve(symbol))
      case Count(symbol)                    => Json.fromInt(toVector(resolve(symbol)).size)
      case Distinct(symbol)                 => Json.fromValues(toVector(resolve(symbol)).distinct)
      case Concat(left, right)              => Json.fromValues(toVector(resolve(left)) ++ toVector(resolve(right)))
      case Cond(predicate, ifTrue, ifFalse) => if (resolvePred(predicate)) resolve(ifTrue) else resolve(ifFalse)

      case Find(symbol, predicate) =>
        toVector(resolve(symbol))
          .find(json => resolvePredicate(json :: lambdaParameterStack, predicate))
          .getOrElse(Json.Null)

      case Add(left, right) =>
        val leftJson  = resolve(left)
        val rightJson = resolve(right)
        if (leftJson.isArray)
          Json.fromValues(toVector(leftJson) :+ rightJson)
        else if (rightJson.isArray)
          Json.fromValues(leftJson +: toVector(rightJson))
        else
          (leftJson.asNumberNullAsZero, rightJson.asNumberNullAsZero)
            .mapN(_ + _)
            .map(Json.fromJsonNumber)
            .getOrElse(
              Json.fromString(
                toStringNoQuotes(leftJson) + toStringNoQuotes(rightJson)
              )
            )

      case Subtract(left, right) =>
        val leftJson  = resolve(left)
        val rightJson = resolve(right)

        if (leftJson.isArray)
          Json.fromValues(toVector(leftJson).filterNot(_ === rightJson))
        else
          (leftJson.asObject, toStringIfPrimitive(rightJson))
            .mapN(_.remove(_))
            .map(Json.fromJsonObject)
            .orElse {
              (leftJson.asNumberNullAsZero, rightJson.asNumberNullAsZero)
                .mapN(_ - _)
                .map(Json.fromJsonNumber)
            }
            .getOrElse(
              Json.fromString(
                "NaN"
              )
            )

      case Multiply(left, right) =>
        (resolve(left).asNumberNullAsZero, resolve(right).asNumberNullAsZero)
          .mapN(_ * _)
          .map(Json.fromJsonNumber)
          .getOrElse(
            Json.fromString(
              "NaN"
            )
          )

      case Divide(left, right) =>
        (resolve(left).asNumberNullAsZero, resolve(right).asNumberNullAsZero)
          .mapN(_ / _)
          .map(Json.fromJsonNumber)
          .getOrElse(
            Json.fromString(
              "NaN"
            )
          )

      case predicate: Predicate => Json.fromBoolean(resolvePredicate(lambdaParameterStack, predicate))
    }
  }

  private def toStringIfPrimitive(json: Json): Option[String] =
    json.fold(
      None,
      _.toString.some,
      _.toString.some,
      _.some,
      _ => None,
      _ => None
    )

  private def toStringNoQuotes(json: Json): String =
    json.asString.getOrElse(json.toString)

  // We use 'Json.Null' instead of 'None: Option[Json]' since optionality may occur _within_ JSON structures too, and
  // we can only use 'Json.Null' there. E.g. 'Map' operations cannot change the size of an array, and since objects in
  // arrays may be different shapes, 'Map' operations may need to yield 'Json.Null' for some elements but not others.
  private def valueAt(json: Json, key: Json): Json =
    json.fold(
      Json.Null,
      _ => Json.Null,
      _ => Json.Null,
      _ => Json.Null,
      _ => Json.Null,
      x => x(toStringNoQuotes(key)).getOrElse(Json.Null)
    )

  private def flatten(json: Json): Json =
    json
      .asArray
      .map(x => Json.fromValues(x.flatMap(toVector)))
      .getOrElse(json)

  private def toVector(json: Json): Vector[Json] =
    json.fold(
      Vector.empty,
      _ => Vector(json),
      _ => Vector(json),
      _ => Vector(json),
      identity,
      x =>
        x.toVector.map {
          case (key, value) =>
            Json.obj(
              "key"   -> Json.fromString(key),
              "value" -> value
            )
        }
    )

}
