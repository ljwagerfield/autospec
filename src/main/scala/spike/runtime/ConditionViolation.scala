package spike.runtime

import spike.schema.Precondition
import spike.SchemaSymbols.Predicate

sealed trait ConditionViolation
object ConditionViolation {
  case class PreconditionViolation(precondition: Precondition) extends ConditionViolation
  case class PostconditionViolation(postcondition: Predicate) extends ConditionViolation
}
