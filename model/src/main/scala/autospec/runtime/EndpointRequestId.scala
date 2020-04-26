package autospec.runtime

import cats.kernel.{Eq, Order}
import autospec.common.ULID

case class EndpointRequestId(sessionId: SessionId, requestIndex: EndpointRequestIndex) {
  def serialized: String = s"${sessionId.value}-${requestIndex.index}"
}

object EndpointRequestId {

  /**
    * Causal ordering is guaranteed within the same session, but not across sessions.
    */
  implicit val ordering: Ordering[EndpointRequestId] =
    Ordering.by[EndpointRequestId, ULID](_.sessionId.value).orElseBy(_.requestIndex)

  implicit val order: Order[EndpointRequestId] = Order.fromOrdering
  implicit val eq: Eq[EndpointRequestId]       = Eq.fromUniversalEquals
}
