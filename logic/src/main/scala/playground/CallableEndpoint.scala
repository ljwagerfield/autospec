package playground

import cats.data.NonEmptyList
import testbot.runtime.EndpointRequest
import testbot.schema.EndpointId

case class CallableEndpoint(endpointId: EndpointId, possibleRequests: NonEmptyList[EndpointRequest])
