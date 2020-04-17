package spike.common

import cats.kernel.{Eq, Order}
import de.huxhorn.sulky.ulid.ULID.{Value => ULIDValue}

case class ULID(value: ULIDValue)

object ULID {
  /**
   * WARNING:
   * ULIDs generated in tight loops may not be causally ordered: the timestamp component has millisecond granularity.
   * For unit tests, use an artificial clock that ticks on every resolve.
   */
  implicit val ordering: Ordering[ULID] = scala.Ordering.by(_.value)
  implicit val order: Order[ULID]       = Order.fromOrdering

  implicit val eq: Eq[ULID] = Eq.fromUniversalEquals
}