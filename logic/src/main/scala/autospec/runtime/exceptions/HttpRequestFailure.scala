package autospec.runtime.exceptions

import autospec.runtime.RequestSummary

/**
  * Occurs due to:
  * - Non-HTTP network errors.
  * - HTTP errors representing infrastructural issues (e.g. 502) that persisted after several retries.
  * - Errors in the HTTP client library itself.
  */
case class HttpRequestFailure(message: String, request: RequestSummary, cause: Throwable)
  extends Exception(message, cause)
