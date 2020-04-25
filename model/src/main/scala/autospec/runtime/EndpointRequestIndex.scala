package autospec.runtime

import cats.kernel.Eq

case class EndpointRequestIndex(index: Long)

object EndpointRequestIndex {
  implicit val eq: Eq[EndpointRequestIndex] = Eq.fromUniversalEquals
  implicit def toRequestIndex(index: Long): EndpointRequestIndex =
    EndpointRequestIndex(index)
  implicit def toRequestIndex(index: Int): EndpointRequestIndex =
    EndpointRequestIndex(index.toLong)
}
