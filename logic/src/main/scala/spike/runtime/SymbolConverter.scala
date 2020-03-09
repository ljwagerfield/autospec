package spike.runtime

import spike.{RuntimeSymbols => R}
import spike.{SchemaSymbols => S}
import cats.implicits._
import alleycats.std.map._
import cats.data.{Chain, ValidatedNec}
import spike.runtime.SymbolConverter.SymbolConverterError.{UnresolvedForwardLookup, UnresolvedReverseLookup}

object SymbolConverter {
  sealed trait SymbolConverterError
  object SymbolConverterError {
    case object UnresolvedForwardLookup extends SymbolConverterError
    case object UnresolvedReverseLookup extends SymbolConverterError
  }

  type SymbolConverterResult[A] = ValidatedNec[SymbolConverterError, A]

  /**
   * Converts a schema predicate to a runtime predicate.
   *
   * @return None if the [[predicate]] references unresolvable requests. Some otherwise.
   */
  def convertToRuntimePredicate(
                                 current: EndpointRequestOld,
                                 currentRequestIndex: Int,
                                 beforeOffset: Int,
                                 before: Chain[EndpointRequestOld],
                                 after: Chain[EndpointRequestOld],
                                 predicate: S.Predicate
  ): SymbolConverterResult[R.Predicate] = {
    val resolveSymbol    = convertToRuntimeSymbol(current, currentRequestIndex, beforeOffset, before, after, _: S.Symbol)
    val resolvePredicate = convertToRuntimePredicate(current, currentRequestIndex, beforeOffset, before, after, _: S.Predicate)
    predicate match {
      case S.Predicate.Equals(left, right)        => (resolveSymbol(left), resolveSymbol(right)).mapN(R.Predicate.Equals)
      case S.Predicate.And(left, right)           => (resolvePredicate(left), resolvePredicate(right)).mapN(R.Predicate.And)
      case S.Predicate.Or(left, right)            => (resolvePredicate(left), resolvePredicate(right)).mapN(R.Predicate.Or)
      case S.Predicate.Not(pred)                  => resolvePredicate(pred).map(R.Predicate.Not)
      case S.Predicate.Exists(symbol, pred)       => (resolveSymbol(symbol), resolvePredicate(pred)).mapN(R.Predicate.Exists)
      case S.Predicate.Contains(collection, item) => (resolveSymbol(collection), resolveSymbol(item)).mapN(R.Predicate.Contains)
    }
  }

  /**
   * Converts a schema symbol to a runtime symbol.
   *
   * @return None if the symbol references unresolvable requests. Some otherwise.
   */
  def convertToRuntimeSymbol(
                              current: EndpointRequestOld,
                              currentRequestIndex: Int,
                              beforeOffset: Int,
                              before: Chain[EndpointRequestOld],
                              after: Chain[EndpointRequestOld],
                              symbol: S.Symbol
  ): SymbolConverterResult[R.Symbol] = {
    val resolveSymbol       = convertToRuntimeSymbol(current, currentRequestIndex, beforeOffset, before, after, _: S.Symbol)
    val resolvePredicate    = convertToRuntimePredicate(current, currentRequestIndex, beforeOffset, before, after, _: S.Predicate)

    symbol match {
      case S.Endpoint(endpointId, schemaParameters, evaluateAfterExecution) =>
        val (scope, unresolved, indexOffset) =
          if (evaluateAfterExecution)
            (after, UnresolvedForwardLookup, currentRequestIndex + 1)
          else
            (before, UnresolvedReverseLookup, beforeOffset)

        def findRequestIndex(r: EndpointRequestOld): SymbolConverterResult[Int] =
          scope
            .zipWithIndex
            .collectFirst { case (request, index) if request === r => index + indexOffset }
            .toValidNec(unresolved)

        val runtimeParameters  = schemaParameters.traverse(resolveSymbol)
        val targetRequest      = runtimeParameters.map(EndpointRequestOld(endpointId, _))
        val targetRequestIndex = targetRequest.andThen(findRequestIndex)

        targetRequestIndex.map(R.ResponseBody)

      case S.ResponseBody              => R.ResponseBody(currentRequestIndex).validNec
      case S.StatusCode                => R.StatusCode(currentRequestIndex).validNec
      case S.Parameter(name)           => current.parameterValue(name).validNec
      case S.Literal(value)            => R.Literal(value).validNec
      case S.LambdaParameter(distance) => R.LambdaParameter(distance).validNec
      case S.Map(symbol, path)         => (resolveSymbol(symbol), resolveSymbol(path)).mapN(R.Map)
      case S.FlatMap(symbol, path)     => (resolveSymbol(symbol), resolveSymbol(path)).mapN(R.FlatMap)
      case S.Flatten(symbol)           => resolveSymbol(symbol).map(R.Flatten)
      case S.Find(symbol, predicate)   => (resolveSymbol(symbol), resolvePredicate(predicate)).mapN(R.Find)
      case S.Count(symbol)             => resolveSymbol(symbol).map(R.Count)
      case S.Distinct(symbol)          => resolveSymbol(symbol).map(R.Distinct)
      case S.Prepend(item, collection) => (resolveSymbol(item), resolveSymbol(collection)).mapN(R.Prepend)
      case S.Append(collection, item)  => (resolveSymbol(collection), resolveSymbol(item)).mapN(R.Append)
      case S.Concat(left, right)       => (resolveSymbol(left), resolveSymbol(right)).mapN(R.Concat)
      case predicate: S.Predicate      => resolvePredicate(predicate)
    }
  }
}
