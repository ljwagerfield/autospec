package autospec.runtime

import autospec.runtime.exceptions.HttpClientException
import autospec.runtime.HttpRequestExecutor._
import cats.data.EitherT
import cats.implicits._
import io.circe._
import io.circe.parser._
import monix.eval.Task
import org.http4s.client.Client
import org.http4s.client.middleware.{Retry, RetryPolicy}
import org.http4s.client.middleware.RetryPolicy.exponentialBackoff
import org.http4s.{Request, Response, Status}

import scala.concurrent.duration._

class HttpRequestExecutor(httpClient: Client[Task]) {

  private val retryingClient: Client[Task] = Retry[Task](
    RetryPolicy(
      exponentialBackoff(5.seconds, maxRetry = 3),
      shouldRetryRequest
    )
  )(httpClient)

  def execute(request: Request[Task]): EitherT[Task, HttpClientException, EndpointResponse] = {
    val result =
      retryingClient.fetch(request) { response =>
        val failedDueToMaxRetryAttempts = shouldRetryRequest(request, response.asRight)
        val bodyAsString                = response.bodyAsText.foldMonoid.compile.lastOrError
        bodyAsString.map { string =>
          Result(
            EndpointResponse(
              response.status.code,
              // We currently assume all API responses are JSON.
              // In future must support other response types, but map to JSON for internal representation.
              parse(string).toOption.getOrElse(Json.Null),
              string
            ),
            failedDueToMaxRetryAttempts
          )
        }
      }

    // HTTP client can thrown exceptions, so convert to Left, as callers want to handle this.
    val resultAsEitherT =
      EitherT(
        result
          .map(_.asRight[HttpClientException])
          .recover { case e => error(request, e).asLeft }
      )

    val response =
      resultAsEitherT.subflatMap {
        case Result(response, failedDueToMaxRetryAttempts) =>
          Either.cond(
            !failedDueToMaxRetryAttempts,
            response,
            error(request, new Exception("Request retried too many times."))
          )
      }

    response
  }

  private def shouldRetryRequest[F[_]](req: Request[F], result: Either[Throwable, Response[F]]): Boolean =
    // Don't retry 500s, as that's considered a failure of the API under test.
    result.forall(_.status =!= Status.InternalServerError) && RetryPolicy.defaultRetriable(
      req,
      result
    )

  private def error(request: Request[Task], exception: Throwable): HttpClientException = {
    val requestSummary = RequestSummary(request)
    HttpClientException(s"Request failed: $requestSummary", requestSummary, exception)
  }

}

object HttpRequestExecutor {

  /**
    * @param failedDueToMaxRetryAttempts If true, discard the [[response]] as it was simply the last response returned
    *                                    in a series of unsuccessful retry attempts (so will be a 504 or similar).
    */
  case class Result(response: EndpointResponse, failedDueToMaxRetryAttempts: Boolean)
}
