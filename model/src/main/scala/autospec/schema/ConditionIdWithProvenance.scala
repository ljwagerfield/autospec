package autospec.schema

import autospec.runtime.EndpointRequestId

/**
 * @param conditionId Condition ID
 * @param provenance ID of the request this condition was checked for.
 */
case class ConditionIdWithProvenance(
  conditionId: ConditionId,
  provenance: EndpointRequestId
)
