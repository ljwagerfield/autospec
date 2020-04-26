package autospec.runtime

import monix.eval.Task

trait EndpointRequestExecutor {
  def execute(session: Session, request: EndpointRequest): Task[EndpointRequestResponse]
}

class EndpointRequestExecutorImpl(httpRequestExecutor: HttpRequestExecutor) extends EndpointRequestExecutor {

  def execute(session: Session, request: EndpointRequest): Task[EndpointRequestResponse] =
    for {
      id            <- session.newRequestId()
      encodedRequest = HttpRequestEncoder.encode[Task](session.schema, request, id)
      response      <- httpRequestExecutor.execute(encodedRequest)
    } yield EndpointRequestResponse(id, request, response)

}
