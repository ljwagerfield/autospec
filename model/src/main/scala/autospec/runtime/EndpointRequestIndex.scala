package autospec.runtime

import cats.kernel.{Eq, Order}

case class EndpointRequestIndex(index: Long) {

  def increment(): EndpointRequestIndex =
    EndpointRequestIndex(index + 1)

}

object EndpointRequestIndex {
  implicit val eq: Eq[EndpointRequestIndex]             = Eq.fromUniversalEquals
  implicit val ordering: Ordering[EndpointRequestIndex] = scala.Ordering.by(_.index)
  implicit val order: Order[EndpointRequestIndex]       = Order.fromOrdering

  implicit def toRequestIndex(index: Long): EndpointRequestIndex =
    EndpointRequestIndex(index)

  implicit def toRequestIndex(index: Int): EndpointRequestIndex =
    EndpointRequestIndex(index.toLong)

}
