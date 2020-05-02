package autospec.runtime

import alleycats.std.all._
import autospec.common.FunctorExtensions._
import autospec.runtime.ConditionStatus.{Failed, Passed}
import autospec.runtime.exceptions.EndpointRequestFailure
import autospec.runtime.resolvers.{RuntimeSymbolResolver, SymbolConverter}
import autospec.schema.SymbolExtensions._
import autospec.schema.{ApplicationSchema, ConditionIdWithProvenance, ConditionIdWithState, EndpointDefinition}
import autospec.{RuntimeSymbolsExecuted => RE, LocalSchemaSymbols => S}
import cats.data.Chain
import cats.implicits._
import fs2.Stream
import io.circe.Json
import monix.eval.Task

import scala.collection.immutable.{Map => ScalaMap}

/**
  * Validates the preconditions and postconditions for each request after it has been executed.
  */
object ResponseValidator {
  private val initialState: State = State(Map.empty, None, Chain.empty)

  private case class Result(
    resolvedConditions: Map[ConditionIdWithProvenance, (ConditionStatus, RE.Predicate)],
    state: State
  )

  private case class State(
    deferredConditions: Map[EndpointRequest, Set[ConditionIdWithState]],
    lastMutatingRequestId: Option[EndpointRequestId],
    history: Chain[EndpointRequestResponse]
  )

  def stream[A, B](schema: ApplicationSchema, responseStream: Stream[Task, Either[A, B]])(
    fa: A => EndpointRequestFailure,
    fb: B => EndpointRequestResponse
  ): Stream[Task, Either[A, (B, ValidatedRequestResponse)]] =
    responseStream
      .mapAccumulate(initialState) { (oldState, responseOrError) =>
        responseOrError.fold(
          e => validateRequestFailure(schema, fa(e).request, fa(e).requestId, oldState) -> e.asLeft,
          r => {
            val Result(result, newState) = validateResponse(schema, oldState, fb(r))
            newState -> (r, ValidatedRequestResponse(fb(r), result)).asRight
          }
        )
      }
      .map(_._2)

  private def validateRequestFailure(
    schema: ApplicationSchema,
    request: EndpointRequest,
    requestId: EndpointRequestId,
    state: State
  ): State =
    updateState(
      state,
      schema.endpoint(request.endpointId),
      requestId,
      request,
      ScalaMap.empty,
      state.history
    )

  private def validateResponse(
    schema: ApplicationSchema,
    state: State,
    newResponse: EndpointRequestResponse
  ): Result = {

    val history = state.history :+ newResponse

    def getResponse(id: EndpointRequestId) =
      history.find(_.requestId === id).getOrElse(throw new Exception("History was truncated incorrectly."))

    def getResponseAfter(after: EndpointRequestId, matching: EndpointRequest) =
      history.findAfter(_.requestId === after, _.request === matching)

    def getResponseBefore(before: EndpointRequestId, upTo: Option[EndpointRequestId], matching: EndpointRequest) = {
      require(
        upTo.forall(_ < before),
        s"The 'upTo' request ${upTo.fold("")(_.serialized)} must appear before the 'before' request ${before.serialized}"
      )
      history
        .reverse
        .findAfter(_.requestId === before, _.request === matching)
        .filter(r => upTo.forall(_ <= r.requestId))
    }

    val endpoint = schema.endpoint(newResponse.request.endpointId)
    val ownConditions = endpoint.conditions.map {
      case (conditionId, predicate) =>
        val requestId             = newResponse.requestId
        val lastMutatingRequestId = state.lastMutatingRequestId
        val reverseLookup         = getResponseBefore(requestId, lastMutatingRequestId, _)
        val earliestDependency = findEarliestDependency(
          schema,
          reverseLookup,
          getResponse(_).response,
          newResponse,
          predicate
        )
        (
          ConditionIdWithState(
            conditionId           = conditionId,
            provenance            = requestId,
            earliestDependency    = earliestDependency,
            lastMutatingRequestId = lastMutatingRequestId
          ),
          predicate
        )
    }
    val request            = newResponse.request
    val deferredConditions = state.deferredConditions.getOrElse(request, Set.empty).toList
    val allConditions      = ownConditions ++ deferredConditions.map(x => x -> schema.condition(x.conditionId)).toMap
    val noForwardLookup    = (_: EndpointRequest) => None: Option[EndpointRequestResponse]

    val conditions =
      allConditions.groupBy { case (conditionId, _) => conditionId.provenance }
        .toList
        .flatMap {
          case (requestId, conditions) =>
            val response = getResponse(requestId)
            conditions.toList.map {
              case (condition, predicate) =>
                val isPrecondition = condition.isPrecondition
                val reverseLookup  = getResponseBefore(requestId, condition.lastMutatingRequestId, _)
                val forwardLookup  = if (isPrecondition) noForwardLookup else getResponseAfter(requestId, _)
                val resolution = resolvePredicate(
                  schema,
                  reverseLookup,
                  forwardLookup,
                  getResponse(_).response,
                  response,
                  predicate
                )
                (
                  condition,
                  resolution
                )
            }
        }

    val (deferredByThisRound, processedOpts) =
      conditions.map {
        case (condition, Right(true -> predicate))  => condition -> Right(Some(Passed -> predicate))
        case (condition, Right(false -> predicate)) => condition -> Right(Some(Failed -> predicate))
        case (condition, Left(request))             => condition -> request.filterNot(_ => condition.isPrecondition).toLeft(None)
      }.toMap.partitionEither(identity)

    // Drop unresolvable conditions (i.e. preconditions that depend on unresolved requests, and thus will never appear).
    val processed = processedOpts.collect { case (key, Some(value)) => key -> value }

    Result(
      processed.collect { case (id, state) => id.withoutState -> state },
      updateState(
        state,
        endpoint,
        newResponse.requestId,
        newResponse.request,
        deferredByThisRound.swap,
        history
      )
    )
  }

  private def updateState(
    state: State,
    endpoint: EndpointDefinition,
    requestId: EndpointRequestId,
    request: EndpointRequest,
    deferredByThisRound: ScalaMap[EndpointRequest, Set[ConditionIdWithState]],
    history: Chain[EndpointRequestResponse]
  ): State = {
    val (oldDeferred, newMutatingRequestId, newHistory) =
      if (endpoint.isMutating)
        (
          ScalaMap.empty[EndpointRequest, Set[ConditionIdWithState]],
          Some(requestId), {
            val earliestDependencies = deferredByThisRound.values.flatten.map(_.earliestDependency).toList.toNel
            val earliestDependency   = earliestDependencies.fold(requestId)(_.minimum)
            history.dropWhile(_.requestId =!= earliestDependency)
          }
        )
      else
        (
          state.deferredConditions,
          state.lastMutatingRequestId,
          history
        )

    val oldDeferredMinusSelf = oldDeferred - request // Remove current request (as it's now been processed)
    val newDeferred          = oldDeferredMinusSelf.merge(deferredByThisRound)

    State(
      newDeferred,
      newMutatingRequestId,
      newHistory
    )
  }

  private def resolvePredicate(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => Option[EndpointRequestResponse],
    getFutureResponse: EndpointRequest => Option[EndpointRequestResponse],
    getResponse: EndpointRequestId => EndpointResponse,
    requestResponse: EndpointRequestResponse,
    predicate: S.Predicate
  ): Either[Option[EndpointRequest], (Boolean, RE.Predicate)] = {
    val convertOwnSymbols =
      convertToRuntimeSymbol(schema, getPastResponse, getFutureResponse, getResponse, requestResponse, _)
    SymbolConverter
      .convertPredicate[Either[Option[EndpointRequest], *], S.type, RE.type](S, RE)(predicate)(convertOwnSymbols)
      .map { runtimePredicate =>
        RuntimeSymbolResolver.resolvePredicate(runtimePredicate, getResponse) -> runtimePredicate
      }
  }

  private def resolveSymbol(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => Option[EndpointRequestResponse],
    getFutureResponse: EndpointRequest => Option[EndpointRequestResponse],
    getResponse: EndpointRequestId => EndpointResponse,
    requestResponse: EndpointRequestResponse,
    symbol: S.Symbol
  ): Either[Option[EndpointRequest], Json] = {
    val convertOwnSymbols =
      convertToRuntimeSymbol(schema, getPastResponse, getFutureResponse, getResponse, requestResponse, _)
    SymbolConverter
      .convertSymbol[Either[Option[EndpointRequest], *], S.type, RE.type](S, RE)(symbol)(convertOwnSymbols)
      .map(RuntimeSymbolResolver.resolveSymbol(_, getResponse))
  }

  private def convertToRuntimeSymbol(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => Option[EndpointRequestResponse],
    getFutureResponse: EndpointRequest => Option[EndpointRequestResponse],
    getResponse: EndpointRequestId => EndpointResponse,
    requestResponse: EndpointRequestResponse,
    symbol: S.OwnSymbols
  ): Either[Option[EndpointRequest], RE.Symbol] =
    symbol match {
      case S.ResponseBody    => RE.ResponseBody(requestResponse.requestId).asRight
      case S.StatusCode      => RE.StatusCode(requestResponse.requestId).asRight
      case S.Parameter(name) => RE.Literal(requestResponse.request.parameterValues(name)).asRight
      case endpoint: S.Endpoint =>
        resolveEndpointReference(
          schema,
          getPastResponse,
          getFutureResponse,
          getResponse,
          requestResponse,
          endpoint
        ).map(x => RE.ResponseBody(x.requestId))
    }

  private def resolveEndpointReference(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => Option[EndpointRequestResponse],
    getFutureResponse: EndpointRequest => Option[EndpointRequestResponse],
    getResponse: EndpointRequestId => EndpointResponse,
    requestResponse: EndpointRequestResponse,
    endpoint: S.Endpoint
  ): Either[Option[EndpointRequest], EndpointRequestResponse] = {
    val resolvedParametersF =
      endpoint
        .parameters
        .traverse(
          resolveSymbol(
            schema,
            getPastResponse,
            getFutureResponse,
            getResponse,
            requestResponse,
            _
          )
        )

    // Do not defer condition if it's dependent on a request that was supposed to have happened in the past:
    val dependsOn = Some(_: EndpointRequest).filter(_ => endpoint.evaluateAfterExecution)

    val responseF = (request: EndpointRequest) =>
      {
        if (endpoint.evaluateAfterExecution)
          getFutureResponse(request)
        else
          getPastResponse(request)
      }.toRight(dependsOn(request))

    for {
      resolvedParameters <- resolvedParametersF
      request             = EndpointRequest(endpoint.endpointId, resolvedParameters)
      response           <- responseF(request)
    } yield response
  }

  private def findEarliestDependency(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => Option[EndpointRequestResponse],
    getResponse: EndpointRequestId => EndpointResponse,
    requestResponse: EndpointRequestResponse,
    predicate: S.Predicate
  ): EndpointRequestId = {
    val reverseLookups =
      predicate.toList.collect { case x: S.Endpoint if !x.evaluateAfterExecution => x }

    val reverseLookupResolutions =
      reverseLookups.map(resolveEndpointReference(schema, getPastResponse, _ => None, getResponse, requestResponse, _))

    val reverseLookupResponses =
      reverseLookupResolutions.flatMap(_.toOption)

    (requestResponse.requestId :: reverseLookupResponses.map(_.requestId)).min
  }

}
