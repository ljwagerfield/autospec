package spike.runtime

import cats.implicits._
import cats.data.NonEmptyList
import spike.RuntimeSymbols.Predicate
import spike.schema.ConditionId

case class EndpointRequestWithChecks(request: EndpointRequest, checks: Map[ConditionId, Predicate]) {
  def validateResponse(history: List[EndpointRequestResponse], response: EndpointResponse): Either[NonEmptyList[ConditionId], EndpointRequestResponse] = {
    val current     = EndpointRequestResponse(request, response)
    val fullHistory = current :: history
    checks
      .view
      .mapValues(SymbolResolver.resolvePredicate(fullHistory, _))
      .toList
      .collect { case (conditionId, satisfied) if !satisfied => conditionId}
      .toNel
      .toLeft(current)
  }
}