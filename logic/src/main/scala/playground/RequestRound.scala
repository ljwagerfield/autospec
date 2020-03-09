package playground

import spike.schema.EndpointId

case class RequestRound(callableEndpoints: Set[EndpointId], chosenRequest: EndpointRequest)
