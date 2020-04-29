package autospec.runtime

import autospec.common.FunctorExtensions._
import autospec.runtime.exceptions.EndpointRequestFailure
import cats.data.EitherT
import monix.eval.Task

trait EndpointRequestExecutor {

  def execute(
    session: Session,
    request: EndpointRequest
  ): EitherT[Task, EndpointRequestFailure, EndpointRequestResponse]

}

class EndpointRequestExecutorImpl(httpRequestExecutor: HttpRequestExecutor) extends EndpointRequestExecutor {

  def execute(
    session: Session,
    request: EndpointRequest
  ): EitherT[Task, EndpointRequestFailure, EndpointRequestResponse] =
    for {
      id            <- session.newRequestId().asRightT[EndpointRequestFailure]
      encodedRequest = HttpRequestEncoder.encode[Task](session.schema, request, id)
      response      <- httpRequestExecutor.execute(encodedRequest).leftMap(e => EndpointRequestFailure(request, id, e))
    } yield EndpointRequestResponse(id, request, response)

}
