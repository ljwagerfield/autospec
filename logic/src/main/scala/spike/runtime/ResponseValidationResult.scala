package spike.runtime

import spike.runtime.ConditionStatus.ResolvedConditionStatus
import spike.schema.ConditionIdWithState

case class ResponseValidationResult(
  completedConditions: Map[ConditionIdWithState, ConditionStatus],
  state: ResponseValidationState
) {
  def resolvedConditions: Map[ConditionIdWithState, ResolvedConditionStatus] =
    completedConditions.collect {
      case (id, status: ResolvedConditionStatus) => id -> status
    }
}
