package spike.runtime

import spike.{RuntimeSymbols => R}
import spike.{SchemaSymbols => S}
import cats.implicits._
import alleycats.std.map._

object SymbolConverter {
  /**
   * Converts a schema predicate to a runtime predicate.
   *
   * @return None if the [[predicate]] references unresolvable requests. Some otherwise.
   */
  def convertToRuntimePredicate(
    current: EndpointRequest,
    before: List[EndpointRequest],
    after: List[EndpointRequest],
    predicate: S.Predicate
  ): Option[R.Predicate] = {
    val resolveSymbol    = convertToRuntimeSymbol(current, before, after, _: S.Symbol)
    val resolvePredicate = convertToRuntimePredicate(current, before, after, _: S.Predicate)
    predicate match {
      case S.Predicate.Equals(left, right) => (resolveSymbol(left), resolveSymbol(right)).mapN(R.Predicate.Equals)
      case S.Predicate.And(left, right)    => (resolvePredicate(left), resolvePredicate(right)).mapN(R.Predicate.And)
      case S.Predicate.Or(left, right)     => (resolvePredicate(left), resolvePredicate(right)).mapN(R.Predicate.Or)
      case S.Predicate.Not(pred)           => resolvePredicate(pred).map(R.Predicate.Not)
    }
  }

  /**
   * Converts a schema symbol to a runtime symbol.
   *
   * @return None if the symbol references unresolvable requests. Some otherwise.
   */
  def convertToRuntimeSymbol(
                              current: EndpointRequest,
                              before: List[EndpointRequest],
                              after: List[EndpointRequest],
                              symbol: S.Symbol
  ): Option[R.Symbol] = {
    val resolveSymbol       = convertToRuntimeSymbol(current, before, after, _: S.Symbol)
    val resolvePredicate    = convertToRuntimePredicate(current, before, after, _: S.Predicate)
    val currentRequestIndex = before.size

    symbol match {
      case S.Endpoint(endpointId, schemaParameters, evaluateAfterExecution) =>
        val (scope, indexOffset) =
          if (evaluateAfterExecution)
            after -> (1 + currentRequestIndex)
          else
            before -> 0

        for {
          runtimeParameters  <- schemaParameters.traverse(resolveSymbol)
          targetRequest       = EndpointRequest(endpointId, runtimeParameters)
          targetRequestIndex <- scope.zipWithIndex.collectFirst {
            case (request, index) if request === targetRequest => index + indexOffset
          }
        } yield {
          R.Result(targetRequestIndex)
        }

      case S.Result                           => Some(R.Result(currentRequestIndex))
      case S.StatusCode                       => Some(R.StatusCode(currentRequestIndex))
      case S.Parameter(name)                  => Some(current.parameterValue(name))
      case S.Literal(value)                   => Some(R.Literal(value))
      case S.LambdaParameter(distance)        => Some(R.LambdaParameter(distance))
      case S.Map(symbol, path)                => resolveSymbol(symbol).map(R.Map(_, path))
      case S.Flatten(symbol)                  => resolveSymbol(symbol).map(R.Flatten)
      case S.FindOne(symbol, predicate)       => (resolveSymbol(symbol), resolvePredicate(predicate)).mapN(R.FindOne)
      case S.Count(symbol)                    => resolveSymbol(symbol).map(R.Count)
      case S.Distinct(symbol)                 => resolveSymbol(symbol).map(R.Distinct)
      case predicate: S.Predicate             => resolvePredicate(predicate)
    }
  }
}
