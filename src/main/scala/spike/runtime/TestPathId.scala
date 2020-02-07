package spike.runtime

import cats.kernel.Eq

case class TestPathId(value: String)

object TestPathId {
  implicit val eq: Eq[TestPathId] = Eq.fromUniversalEquals
}