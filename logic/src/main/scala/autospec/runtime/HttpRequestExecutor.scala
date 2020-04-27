package autospec.runtime

import cats.implicits._
import io.circe._
import io.circe.parser._
import monix.eval.Task
import org.http4s.client.Client
import org.http4s.client.middleware.{Retry, RetryPolicy}
import org.http4s.{Request, Response, Status}

import scala.concurrent.duration._

class HttpRequestExecutor(httpClient: Client[Task]) {

  private val retryingClient: Client[Task] = Retry[Task](
    RetryPolicy(
      RetryPolicy.exponentialBackoff(5.seconds, maxRetry = 3),
      defaultRetriableOrExceptions
    )
  )(httpClient)

  def execute(request: Request[Task]): Task[EndpointResponse] =
    retryingClient.fetch(request) { response =>
      val bodyAsString = response.bodyAsText.foldMonoid.compile.lastOrError
      bodyAsString.map { string =>
        EndpointResponse(
          response.status.code,
          // Currently coupled to JSON. In future could support XML etc. but map to JSON for internal representation.
          parse(string).toOption.getOrElse(Json.Null),
          string
        )
      }
    }.adaptError {
      case e => new Exception(s"Failed to execute request '${request.method.name} ${request.uri.toString()}'", e)
    }

  // Todo: replace this with just 'RetryPolicy.defaultRetriable' (see: https://trello.com/c/Opnd483A/28-handle-request-failures)
  private def defaultRetriableOrExceptions[F[_]](req: Request[F], result: Either[Throwable, Response[F]]): Boolean =
    // Don't retry 500s, as that's considered a failure AutoSpec wants to flag.
    result.forall(_.status =!= Status.InternalServerError) && (RetryPolicy.defaultRetriable(
      req,
      result
    ) || result.isLeft)

}
