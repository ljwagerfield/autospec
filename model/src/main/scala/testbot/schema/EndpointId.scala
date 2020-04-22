package testbot.schema

import cats.kernel.Eq

case class EndpointId(value: String)

object EndpointId {
  implicit val eq: Eq[EndpointId] = Eq.fromUniversalEquals
}
