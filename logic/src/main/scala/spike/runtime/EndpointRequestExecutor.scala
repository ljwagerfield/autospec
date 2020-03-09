package spike.runtime

import monix.eval.Task
import playground.{EndpointRequest, EndpointRequestResponse, HttpRequestEncoder}
import spike.schema.ApplicationSchema

class EndpointRequestExecutor(httpRequestExecutor: HttpRequestExecutor) {
  def execute(schema: ApplicationSchema, request: EndpointRequest): Task[EndpointRequestResponse] = {
    val httpRequest = HttpRequestEncoder.encode[Task](schema, request)
    httpRequestExecutor.execute(httpRequest).map { response =>
      EndpointRequestResponse(request, response)
    }
  }
}
