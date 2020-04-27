package autospec.runtime

import autospec.runtime.exceptions.HttpClientException
import cats.data.EitherT
import monix.eval.Task
import autospec.common.FunctorExtensions._

trait EndpointRequestExecutor {
  def execute(session: Session, request: EndpointRequest): EitherT[Task, HttpClientException, EndpointRequestResponse]
}

class EndpointRequestExecutorImpl(httpRequestExecutor: HttpRequestExecutor) extends EndpointRequestExecutor {

  def execute(
    session: Session,
    request: EndpointRequest
  ): EitherT[Task, HttpClientException, EndpointRequestResponse] =
    for {
      id            <- session.newRequestId().asRightT[HttpClientException]
      encodedRequest = HttpRequestEncoder.encode[Task](session.schema, request, id)
      response      <- httpRequestExecutor.execute(encodedRequest)
    } yield EndpointRequestResponse(id, request, response)

}
