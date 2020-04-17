package spike.runtime

import alleycats.std.all._
import cats.data.Chain
import cats.implicits._
import fs2.Stream
import monix.eval.Task
import spike.runtime.ConditionStatus.{ResolvedConditionStatus, Unresolvable}
import spike.runtime.ResponseValidator.ResponseValidationState
import spike.runtime.TestPathExecutor.ValidatedRequestResponse
import spike.runtime.resolvers.RuntimeSymbolResolver
import spike.schema.{ApplicationSchema, ConditionId}

class TestPathExecutor(requestExecutor: EndpointRequestExecutor) {

  def execute(schema: ApplicationSchema, paths: List[TestPathOld]): Task[Map[TestPathId, List[ValidatedRequestResponse]]] =
    paths.map(x => x.id -> x).toMap.traverse(execute(schema, _))

  def execute(schema: ApplicationSchema, path: TestPathOld): Task[List[ValidatedRequestResponse]] = {
    val initialState     = Chain.empty[EndpointResponse] -> ResponseValidationState.initial
    val requestStream    = Stream.emits[Task, EndpointRequestSymbolic](path.requests.toList)
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
          requestSymbolic    = requestSymbolic,
          response           = response.response,
          resolvedConditions = validationResult.resolvedConditions.map { case (id, status) => id.conditionId -> status }
        )
        newStreamState -> streamItem
      }
    }
    validationStream.map(_._2).takeThrough(!_.isFailed).compile.toList
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
    requestSymbolic: EndpointRequestSymbolic,
    response: EndpointResponse,
    resolvedConditions: Map[ConditionId, ResolvedConditionStatus]
  ) {
    def allConditions(schema: ApplicationSchema): Map[ConditionId, ConditionStatus] = {
      val endpoint     = schema.endpoint(request.endpointId)
      val conditionIds = endpoint.conditions.keys
      conditionIds.map(id => id -> resolvedConditions.getOrElse(id, Unresolvable)).toMap
    }

    def isFailed: Boolean =
      resolvedConditions.values.exists(_.isFailed)
  }
}