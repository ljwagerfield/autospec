package playground

import autospec.runtime.{EndpointRequest, EndpointRequestId, EndpointResponse}

case class EndpointRequestResponse(
  requestId: EndpointRequestId,
  request: EndpointRequest,
  response: EndpointResponse
)
