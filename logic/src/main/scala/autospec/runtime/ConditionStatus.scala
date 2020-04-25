package autospec.runtime

import autospec.runtime.ConditionStatus.{Failed, Passed}

sealed trait ConditionStatus extends Product with Serializable {

  def isFailed: Boolean =
    this match {
      case Failed => true
      case Passed => false
    }
}

object ConditionStatus {
  case object Passed extends ConditionStatus
  case object Failed extends ConditionStatus
}
