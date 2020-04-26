package autospec.runtime

case class EndpointRequestResponse(
  requestId: EndpointRequestId,
  request: EndpointRequest,
  response: EndpointResponse
)
