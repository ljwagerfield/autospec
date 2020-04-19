package spike.runtime

import cats.data.Chain
import playground.EndpointRequestResponse
import spike.schema.ConditionIdWithState

case class ResponseValidationState(
  deferredConditions: Map[EndpointRequest, Set[ConditionIdWithState]],
  lastMutatingRequestId: Option[EndpointRequestId],
  history: Chain[EndpointRequestResponse]
)

object ResponseValidationState {
  val initial: ResponseValidationState = ResponseValidationState(Map.empty, None, Chain.empty)
}
