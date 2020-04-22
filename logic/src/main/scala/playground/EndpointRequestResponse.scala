package playground

import testbot.runtime.{EndpointRequest, EndpointRequestId, EndpointResponse}

case class EndpointRequestResponse(
  requestId: EndpointRequestId,
  request: EndpointRequest,
  response: EndpointResponse
)
