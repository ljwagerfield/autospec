package autospec.schema

import autospec.schema.ConditionType.Precondition
import autospec.schema.ConditionType.Postcondition

sealed trait ConditionType {
  def isPrecondition: Boolean = this match {
    case Precondition  => true
    case Postcondition => false
  }

  def isPostcondition: Boolean = this match {
    case Precondition  => false
    case Postcondition => true
  }
}

object ConditionType {
  case object Precondition extends ConditionType
  case object Postcondition extends ConditionType
}
