package playground

import cats.data.NonEmptyList
import spike.runtime.EndpointRequest
import spike.schema.EndpointId

case class CallableEndpoint(endpointId: EndpointId, possibleRequests: NonEmptyList[EndpointRequest])
