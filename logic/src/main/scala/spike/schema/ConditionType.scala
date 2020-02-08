package spike.schema

sealed trait ConditionType

object ConditionType {
  case object Precondition extends ConditionType
  case object Postcondition extends ConditionType
}
