package spike.runtime.http

import monix.eval.Task
import org.http4s.Request
import org.http4s.client.Client
import spike.runtime.{EndpointResponse, StructuredData}
import org.http4s.circe._
import io.circe._

class HttpRequestExecutor(httpClient: Client[Task]) {
  def apply(request: Request[Task]): Task[EndpointResponse] =
    httpClient.fetch(request) { response =>
      response
        .as[Json] // Currently coupled to JSON. In future could support XML etc. but map to JSON for internal representation.
        .onErrorRecover { case _ => Json.Null }
        .map { json =>
          EndpointResponse(
            response.status.code,
            StructuredData(json)
          )
        }
    }
}
