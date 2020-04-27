package autospec.runtime

import monix.eval.Task
import org.http4s.{Method, Request, Uri}

/**
  * Provides a consistent format when logging HTTP requests.
  *
  * E.g. logger.debug(s"Failed request: ${RequestSummary(request)}")
  */
case class RequestSummary(method: Method, uri: Uri) {

  override def toString: String =
    s"${method.name} ${uri.toString()}"

}

object RequestSummary {

  def apply(request: Request[Task]): RequestSummary =
    RequestSummary(request.method, request.uri)

}
