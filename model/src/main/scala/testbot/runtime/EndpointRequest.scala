package testbot.runtime

import cats.kernel.Eq
import io.circe.Json
import testbot.schema.{EndpointId, EndpointParameterName}

case class EndpointRequest(endpointId: EndpointId, parameterValues: Map[EndpointParameterName, Json]) {
  def parameterValue(name: EndpointParameterName): Json =
    parameterValues.getOrElse(name, throw new Exception(s"Cannot find value for parameter '${name.value}' for endpoint '${endpointId.value}' in test path."))

  def prettyPrint: String =
    s"${endpointId.value}(${parameterValues.toList.map(x => s"${x._1.value} = ${x._2}").mkString(", ")})"
}

object EndpointRequest {
  implicit val eq: Eq[EndpointRequest] = Eq.fromUniversalEquals
}
