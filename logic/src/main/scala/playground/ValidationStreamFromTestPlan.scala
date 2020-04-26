package playground

import cats.data.Chain
import fs2.Stream
import monix.eval.Task
import autospec.runtime.resolvers.RuntimeSymbolResolver
import autospec.runtime._
import autospec.schema.ApplicationSchema

class ValidationStreamFromTestPlan(requestExecutor: EndpointRequestExecutor) {

  def apply(
    session: Session,
    path: List[EndpointRequestSymbolic]
  ): Stream[Task, ValidatedRequestResponseWithSymbols] =
    requestStream(path)
      .through(responseStream(session))
      .through(validationStream(session.schema))

  private def requestStream(path: List[EndpointRequestSymbolic]): Stream[Task, EndpointRequestSymbolic] =
    Stream.emits[Task, EndpointRequestSymbolic](path)

  private def responseStream(session: Session)(
    requestStream: Stream[Task, EndpointRequestSymbolic]
  ): Stream[Task, (EndpointRequestSymbolic, EndpointRequestResponse)] =
    requestStream
      .evalMapAccumulate(Chain.empty[EndpointResponse]) { (oldHistory, requestSymbolic) =>
        val request = resolveRequestSymbols(requestSymbolic, oldHistory)
        requestExecutor.execute(session, request).map { response =>
          val newHistory = oldHistory :+ response.response
          newHistory -> (requestSymbolic -> response)
        }
      }
      .map(_._2)

  private def validationStream(schema: ApplicationSchema)(
    responseStream: Stream[Task, (EndpointRequestSymbolic, EndpointRequestResponse)]
  ): Stream[Task, ValidatedRequestResponseWithSymbols] =
    ResponseValidator
      .stream(schema, responseStream)(_._2)
      .map {
        case ((requestSymbolic, _), validatedResponse) =>
          ValidatedRequestResponseWithSymbols(
            validatedResponse,
            requestSymbolic
          )
      }

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
