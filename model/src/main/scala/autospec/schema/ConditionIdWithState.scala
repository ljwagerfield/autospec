package autospec.schema

import autospec.runtime.EndpointRequestId

/**
  * @param conditionId Condition ID
  * @param provenance ID of the request this condition is being checked for.
  * @param earliestDependency ID of the earliest request this condition has a dependency on.
  * @param lastMutatingRequestId ID of the previous request to the [[provenance]] request that mutated state.
  */
case class ConditionIdWithState(
  conditionId: ConditionId,
  provenance: EndpointRequestId,
  earliestDependency: EndpointRequestId,
  lastMutatingRequestId: Option[EndpointRequestId]
) {
  def isPrecondition: Boolean = conditionId.conditionType.isPrecondition

  def withoutState: ConditionIdWithProvenance =
    ConditionIdWithProvenance(
      conditionId,
      provenance
    )
}
