package spike.runtime

import alleycats.std.all._
import cats.data.Chain
import cats.implicits._
import fs2.Stream
import monix.eval.Task
import spike.runtime.ConditionStatus.ResolvedConditionStatus
import spike.runtime.TestPathExecutor.ValidatedRequestResponse
import spike.runtime.resolvers.RuntimeSymbolResolver
import spike.schema.{ApplicationSchema, ConditionIdWithProvenance}

class TestPathExecutor(requestExecutor: EndpointRequestExecutor) {

  def executeMany(schema: ApplicationSchema, paths: List[TestPath], haltOnFailure: Boolean): Task[Map[TestPathId, List[ValidatedRequestResponse]]] =
    paths.map(x => x.id -> x).toMap.traverse(execute(schema, _, haltOnFailure))

  def execute(schema: ApplicationSchema, path: TestPath, haltOnFailure: Boolean): Task[List[ValidatedRequestResponse]] =
    execute(schema, path.requests.toList, haltOnFailure)

  def execute(schema: ApplicationSchema, path: List[EndpointRequestSymbolic], haltOnFailure: Boolean): Task[List[ValidatedRequestResponse]] = {
    val source   = stream(schema, path)
    val filtered = if (haltOnFailure) source.takeThrough(!_.isFailed) else source
    filtered.compile.toList
  }

  def stream(schema: ApplicationSchema, path: List[EndpointRequestSymbolic]): Stream[Task, ValidatedRequestResponse] = {
    val initialState     = Chain.empty[EndpointResponse] -> ResponseValidationState.initial
    val requestStream    = Stream.emits[Task, EndpointRequestSymbolic](path)
    val validationStream = requestStream.evalMapAccumulate(initialState) { (oldStreamState, requestSymbolic) =>
      val (oldHistory, oldValidationState) = oldStreamState
      val request                          = resolveRequestSymbols(requestSymbolic, oldHistory)
      requestExecutor.execute(schema, request).map { response =>
        val validationResult   = ResponseValidator.validateConditions(schema, oldValidationState, response)
        val newHistory         = oldHistory :+ response.response
        val newValidationState = validationResult.state
        val newStreamState     = (newHistory, newValidationState)
        val streamItem         = ValidatedRequestResponse(
          request            = request,
          requestId          = response.requestId,
          requestSymbolic    = requestSymbolic,
          response           = response.response,
          resolvedConditions = validationResult.resolvedConditions.map { case (id, status) => id.withoutState -> status }
        )
        newStreamState -> streamItem
      }
    }
    validationStream.map(_._2)
  }

  private def resolveRequestSymbols(request: EndpointRequestSymbolic, history: Chain[EndpointResponse]): EndpointRequest =
    EndpointRequest(
      request.endpointId,
      request
        .parameterValues
        .view
        .mapValues(RuntimeSymbolResolver.resolveSymbol(_, history))
        .toMap
    )
}

object TestPathExecutor {
  case class ValidatedRequestResponse(
    request: EndpointRequest,
    requestId: EndpointRequestId,
    requestSymbolic: EndpointRequestSymbolic,
    response: EndpointResponse,
    resolvedConditions: Map[ConditionIdWithProvenance, ResolvedConditionStatus]
  ) {
    def isFailed: Boolean =
      resolvedConditions.values.exists(_.isFailed)
  }
}