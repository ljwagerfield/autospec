package spike.runtime.http

import monix.eval.Task
import org.http4s.Request
import org.http4s.client.Client
import spike.runtime.EndpointResponse
import io.circe._
import io.circe.parser._
import cats.implicits._

class HttpRequestExecutor(httpClient: Client[Task]) {
  def apply(request: Request[Task]): Task[EndpointResponse] =
    httpClient.fetch(request) { response =>
      val bodyAsString = response.bodyAsText.foldMonoid.compile.lastOrError
      bodyAsString.map { string =>
        EndpointResponse(
          response.status.code,
          parse(string).toOption.getOrElse(Json.Null), // Currently coupled to JSON. In future could support XML etc. but map to JSON for internal representation.
          string
        )
      }
    }
}
