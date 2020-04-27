package autospec.runtime.exceptions

import autospec.runtime.{EndpointRequestSymbolic, RequestSummary}

/**
  * Occurs due to:
  * - Non-HTTP network errors.
  * - HTTP errors representing infrastructural issues (e.g. 502) that persisted after several retries.
  * - Errors in the HTTP client library itself.
  */
case class HttpClientException(message: String, request: RequestSummary, cause: Throwable)
  extends Exception(message, cause) {

  def withSymbols(request: EndpointRequestSymbolic): HttpClientExceptionWithSymbols =
    HttpClientExceptionWithSymbols(request, this)

}
