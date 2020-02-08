package spike.runtime

import cats.implicits._
import cats.data.NonEmptyList
import spike.schema.ConditionId

object ResponseValidator {
  def validateResponse(history: List[EndpointRequestResponse], request: EndpointRequestWithChecks, response: EndpointResponse): Either[NonEmptyList[ConditionId], EndpointRequestResponse] = {
    val current     = EndpointRequestResponse(request.request, response)
    val fullHistory = current :: history
    request
      .checks
      .view
      .mapValues(SymbolResolver.resolvePredicate(fullHistory, _))
      .toList
      .collect { case (conditionId, satisfied) if !satisfied => conditionId}
      .toNel
      .toLeft(current)
  }
}
