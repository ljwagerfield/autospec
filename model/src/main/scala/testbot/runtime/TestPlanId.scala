package testbot.runtime

import cats.kernel.Eq

case class TestPlanId(value: String)

object TestPlanId {
  implicit val eq: Eq[TestPlanId] = Eq.fromUniversalEquals
}
