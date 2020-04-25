package autospec.runtime

import cats.implicits._
import io.circe._
import io.circe.parser._
import monix.eval.Task
import org.http4s.Request
import org.http4s.client.Client

class HttpRequestExecutor(httpClient: Client[Task]) {

  def execute(request: Request[Task]): Task[EndpointResponse] =
    httpClient.fetch(request) { response =>
      val bodyAsString = response.bodyAsText.foldMonoid.compile.lastOrError
      bodyAsString.map { string =>
        EndpointResponse(
          response.status.code,
          // Currently coupled to JSON. In future could support XML etc. but map to JSON for internal representation.
          parse(string).toOption.getOrElse(Json.Null),
          string
        )
      }
    }
}
