package autospec.runtime

import autospec.schema.ConditionIdWithProvenance
import autospec.{RuntimeSymbolsExecuted => R}

case class ResponseValidationResult(
  resolvedConditions: Map[ConditionIdWithProvenance, (ConditionStatus, R.Predicate)],
  state: ResponseValidationState
)
