package autospec.runtime

import cats.kernel.{Eq, Order}
import autospec.common.ULID

case class EndpointRequestId(value: ULID)

object EndpointRequestId {
  implicit val eq: Eq[EndpointRequestId]             = Eq.fromUniversalEquals
  implicit val ordering: Ordering[EndpointRequestId] = scala.Ordering.by(_.value)
  implicit val order: Order[EndpointRequestId]       = Order.fromOrdering
}
