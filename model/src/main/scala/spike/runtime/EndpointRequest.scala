package spike.runtime

import cats.kernel.Eq
import io.circe.Json
import spike.schema.{EndpointId, EndpointParameterName}

case class EndpointRequest(endpointId: EndpointId, parameterValues: Map[EndpointParameterName, Json]) {
  def parameterValue(name: EndpointParameterName): Json =
    parameterValues.getOrElse(name, throw new Exception(s"Cannot find value for parameter '${name.value}' for endpoint '${endpointId.value}' in test path."))
}

object EndpointRequest {
  implicit val eq: Eq[EndpointRequest] = Eq.fromUniversalEquals
}