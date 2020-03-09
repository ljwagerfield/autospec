package spike.schema

import cats.kernel.Eq

case class EndpointParameterName(value: String)

object EndpointParameterName {
  implicit val eq: Eq[EndpointParameterName] = Eq.fromUniversalEquals
}
