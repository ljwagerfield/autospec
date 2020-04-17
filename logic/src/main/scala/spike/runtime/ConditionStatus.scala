package spike.runtime

import spike.runtime.ConditionStatus.{Failed, Passed, Unresolvable}

sealed trait ConditionStatus extends Product with Serializable {
  def isFailed: Boolean = this match {
    case Failed                => true
    case Unresolvable | Passed => false
  }
}

object ConditionStatus {
  sealed trait ResolvedConditionStatus extends ConditionStatus

  case object Passed extends ResolvedConditionStatus
  case object Failed extends ResolvedConditionStatus

  case object Unresolvable extends ConditionStatus
}
