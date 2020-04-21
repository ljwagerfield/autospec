package spike.runtime

import spike.runtime.ConditionStatus.ResolvedConditionStatus
import spike.schema.ConditionIdWithProvenance

case class ResponseValidationResult(
  resolvedConditions: Map[ConditionIdWithProvenance, ResolvedConditionStatus],
  state: ResponseValidationState
)