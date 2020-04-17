package playground

import spike.runtime.{EndpointRequest, EndpointRequestId, EndpointResponse}

case class EndpointRequestResponse(
  requestId: EndpointRequestId,
  request: EndpointRequest,
  response: EndpointResponse
)