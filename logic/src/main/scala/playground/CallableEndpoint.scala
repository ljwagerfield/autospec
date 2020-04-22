package playground

import cats.data.NonEmptyList
import autospec.runtime.EndpointRequest
import autospec.schema.EndpointId

case class CallableEndpoint(endpointId: EndpointId, possibleRequests: NonEmptyList[EndpointRequest])
