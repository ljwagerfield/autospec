package spike.schema

import spike.runtime.EndpointRequestId

/**
 * @param conditionId Condition ID
 * @param provenance ID of the request this condition was checked for.
 */
case class ConditionIdWithProvenance(
  conditionId: ConditionId,
  provenance: EndpointRequestId
)
