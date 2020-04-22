package autospec.runtime

import autospec.runtime.ConditionStatus.ResolvedConditionStatus
import autospec.schema.ConditionIdWithProvenance

case class ResponseValidationResult(
  resolvedConditions: Map[ConditionIdWithProvenance, ResolvedConditionStatus],
  state: ResponseValidationState
)
