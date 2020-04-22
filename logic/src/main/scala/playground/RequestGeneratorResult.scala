package playground

import testbot.runtime.EndpointRequest

case class RequestGeneratorResult(
  opportunities: Opportunities,
  nextRequest: EndpointRequest
)
