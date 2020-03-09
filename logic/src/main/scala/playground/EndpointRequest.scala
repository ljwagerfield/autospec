package playground

import io.circe.Json
import spike.schema.{EndpointId, EndpointParameterName}

case class EndpointRequest(endpointId: EndpointId, parameterValues: Map[EndpointParameterName, Json])
