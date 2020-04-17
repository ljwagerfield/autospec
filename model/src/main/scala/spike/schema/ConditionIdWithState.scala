package spike.schema

import spike.runtime.EndpointRequestId

/**
 * @param conditionId Condition ID
 * @param provenance ID of the request this condition is being checked for.
 */
case class ConditionIdWithState(
  conditionId: ConditionId,
  provenance: EndpointRequestId,
  earliestDependency: EndpointRequestId,
  lastMutatingRequestId: Option[EndpointRequestId]
) {
  def isPrecondition: Boolean = conditionId.conditionType.isPrecondition
}
