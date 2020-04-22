package testbot.runtime

import testbot.runtime.ConditionStatus.ResolvedConditionStatus
import testbot.schema.ConditionIdWithProvenance

case class ResponseValidationResult(
  resolvedConditions: Map[ConditionIdWithProvenance, ResolvedConditionStatus],
  state: ResponseValidationState
)
