package spike.runtime

import cats.kernel.Eq
import spike.RuntimeSymbols._
import spike.schema.{EndpointId, EndpointParameterName}

case class EndpointRequestSymbolic(endpointId: EndpointId, parameterValues: scala.collection.immutable.Map[EndpointParameterName, Symbol]) {
  def parameterValue(name: EndpointParameterName): Symbol =
    parameterValues.getOrElse(name, throw new Exception(s"Cannot find value for parameter '${name.value}' for endpoint '${endpointId.value}' in test path."))
}

object EndpointRequestSymbolic {
  implicit val eq: Eq[EndpointRequestSymbolic] = Eq.fromUniversalEquals
}