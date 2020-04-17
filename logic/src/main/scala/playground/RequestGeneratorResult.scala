package playground

import spike.runtime.EndpointRequest

case class RequestGeneratorResult(
  opportunities: Opportunities,
  nextRequest: EndpointRequest
)
