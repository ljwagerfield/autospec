package spike.runtime

import cats.data.NonEmptyList
import spike.RuntimeSymbols.Predicate
import spike.schema.ConditionId

case class EndpointRequestWithChecks(request: EndpointRequest, checks: Map[ConditionId, Predicate]) {
  def validateResponse(history: List[EndpointRequestResponse], response: EndpointResponse): Either[NonEmptyList[ConditionId], EndpointRequestResponse] = {
    // Do this 2nd.

    // Right(EndpointRequestResponse(request, response)) -- if success
  }
}