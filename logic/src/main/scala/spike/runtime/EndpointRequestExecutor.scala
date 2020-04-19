package spike.runtime

import monix.eval.Task
import playground.{EndpointRequestResponse, HttpRequestEncoder}
import spike.common.ULID
import spike.schema.ApplicationSchema

trait EndpointRequestExecutor {
  def execute(schema: ApplicationSchema, request: EndpointRequest): Task[EndpointRequestResponse]
}

class EndpointRequestExecutorImpl(httpRequestExecutor: HttpRequestExecutor) extends EndpointRequestExecutor {
  def execute(schema: ApplicationSchema, request: EndpointRequest): Task[EndpointRequestResponse] =
    for {
      id             <- ULID.next[Task].map(EndpointRequestId.apply)
      encodedRequest  = HttpRequestEncoder.encode[Task](schema, request)
      response       <- httpRequestExecutor.execute(encodedRequest)
    } yield {
      EndpointRequestResponse(id, request, response)
    }
}
