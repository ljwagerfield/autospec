package autospec.runtime.exceptions

import autospec.runtime.{EndpointRequest, EndpointRequestId, EndpointRequestSymbolic}

case class EndpointRequestFailure(request: EndpointRequest, requestId: EndpointRequestId, cause: HttpRequestFailure)
  extends Exception(cause) {

  def withSymbols(request: EndpointRequestSymbolic): EndpointRequestFailureWithSymbols =
    EndpointRequestFailureWithSymbols(request, this)

}
