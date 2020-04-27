package autospec.runtime.exceptions

import autospec.runtime.EndpointRequestSymbolic

/**
  * Occurs due to:
  * - Non-HTTP network errors.
  * - HTTP errors representing infrastructural issues (e.g. 502) that persisted after several retries.
  * - Errors in the HTTP client library itself.
  */
case class HttpClientExceptionWithSymbols(request: EndpointRequestSymbolic, cause: HttpClientException)
  extends Exception(cause)
