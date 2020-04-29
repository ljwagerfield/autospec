package autospec.runtime

import autospec.runtime.exceptions.EndpointRequestFailureWithSymbols
import autospec.runtime.resolvers.RuntimeSymbolResolver
import autospec.schema.ApplicationSchema
import cats.data.Chain
import cats.implicits._
import fs2.Stream
import monix.eval.Task

class ValidatedStreamFromRequestStream(requestExecutor: EndpointRequestExecutor) {

  def apply(
    session: Session
  )(
    requestStream: Stream[Task, EndpointRequestSymbolic]
  ): Stream[Task, Either[EndpointRequestFailureWithSymbols, ValidatedRequestResponseWithSymbols]] =
    requestStream
      .through(responseStream(session))
      .through(validationStream(session.schema))

  private def responseStream(session: Session)(
    requestStream: Stream[Task, EndpointRequestSymbolic]
  ): Stream[Task, Either[EndpointRequestFailureWithSymbols, (EndpointRequestSymbolic, EndpointRequestResponse)]] =
    requestStream
      .evalMapAccumulate(Chain.empty[EndpointResponse]) { (oldHistory, requestSymbolic) =>
        val request = resolveRequestSymbols(requestSymbolic, oldHistory)
        requestExecutor
          .execute(session, request)
          .map { response =>
            val newHistory = oldHistory :+ response.response
            newHistory -> (requestSymbolic -> response).asRight[EndpointRequestFailureWithSymbols]
          }
          .valueOr(error => oldHistory -> error.withSymbols(requestSymbolic).asLeft)
      }
      .map(_._2)

  private def validationStream(schema: ApplicationSchema)(
    responseStream: Stream[
      Task,
      Either[EndpointRequestFailureWithSymbols, (EndpointRequestSymbolic, EndpointRequestResponse)]
    ]
  ): Stream[Task, Either[EndpointRequestFailureWithSymbols, ValidatedRequestResponseWithSymbols]] =
    ResponseValidator.stream(schema, responseStream)(_.cause, _._2)
      .map(_.map {
        case ((requestSymbolic, _), validatedResponse) =>
          ValidatedRequestResponseWithSymbols(
            validatedResponse,
            requestSymbolic
          )
      })

  private def resolveRequestSymbols(
    request: EndpointRequestSymbolic,
    history: Chain[EndpointResponse]
  ): EndpointRequest =
    EndpointRequest(
      request.endpointId,
      request
        .parameterValues
        .view
        .mapValues(RuntimeSymbolResolver.resolveSymbol(_, history))
        .toMap
    )

}
