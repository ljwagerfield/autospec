package autospec.runtime.exceptions

import autospec.runtime.EndpointRequestSymbolic

case class EndpointRequestFailureWithSymbols(request: EndpointRequestSymbolic, cause: EndpointRequestFailure)
  extends Exception(cause)
