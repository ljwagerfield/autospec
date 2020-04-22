package playground

import autospec.runtime.EndpointRequest

case class RequestGeneratorResult(
  opportunities: Opportunities,
  nextRequest: EndpointRequest
)
