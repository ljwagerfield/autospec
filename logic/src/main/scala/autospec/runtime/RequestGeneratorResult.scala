package autospec.runtime

case class RequestGeneratorResult(
  opportunities: Opportunities,
  nextRequest: EndpointRequest
)
