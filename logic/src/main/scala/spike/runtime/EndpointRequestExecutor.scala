package spike.runtime

import monix.eval.Task
import playground.{EndpointRequestResponse, HttpRequestEncoder}
import spike.common.ULIDFactory
import spike.schema.ApplicationSchema

class EndpointRequestExecutor(httpRequestExecutor: HttpRequestExecutor, ulid: ULIDFactory) {
  def execute(schema: ApplicationSchema, request: EndpointRequest): Task[EndpointRequestResponse] =
    for {
      id             <- ulid.nextULID[Task].map(EndpointRequestId.apply)
      encodedRequest  = HttpRequestEncoder.encode[Task](schema, request)
      response       <- httpRequestExecutor.execute(encodedRequest)
    } yield {
      EndpointRequestResponse(id, request, response)
    }
}
