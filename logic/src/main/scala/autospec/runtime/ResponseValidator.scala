package autospec.runtime

import alleycats.std.all._
import autospec.common.FunctorExtensions._
import autospec.runtime.ConditionStatus.{Failed, Passed}
import autospec.runtime.resolvers.{RuntimeSymbolResolver, SymbolConverter}
import autospec.schema.SymbolExtensions._
import autospec.schema.{ApplicationSchema, ConditionIdWithProvenance, ConditionIdWithState}
import autospec.{RuntimeSymbolsExecuted => RE, SchemaSymbols => S}
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

  def stream[A](schema: ApplicationSchema, responseStream: Stream[Task, A])(
    f: A => EndpointRequestResponse
  ): Stream[Task, (A, ValidatedRequestResponse)] =
    responseStream
      .mapAccumulate(initialState) { (oldState, response) =>
        val Result(result, newState) = validate(schema, oldState, f(response))
        (newState, (response, ValidatedRequestResponse(f(response), result)))
      }
      .map(_._2)

  private def validate(
    schema: ApplicationSchema,
    state: State,
    requestResponse: EndpointRequestResponse
  ): Result = {
    val history = state.history :+ requestResponse

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

    val endpoint = schema.endpoint(requestResponse.request.endpointId)
    val ownConditions = endpoint.conditions.map {
      case (conditionId, predicate) =>
        val requestId             = requestResponse.requestId
        val lastMutatingRequestId = state.lastMutatingRequestId
        val reverseLookup         = getResponseBefore(requestId, lastMutatingRequestId, _)
        val earliestDependency = findEarliestDependency(
          schema,
          reverseLookup,
          getResponse(_).response,
          requestResponse,
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
    val request            = requestResponse.request
    val deferredConditions = state.deferredConditions.getOrElse(request, Set.empty).toList
    val allConditions      = ownConditions ++ deferredConditions.map(x => x -> schema.condition(x.conditionId)).toMap
    val noForwardLookup    = (_: EndpointRequest) => None: Option[EndpointRequestResponse]

    val nextMutatingRequestId =
      if (endpoint.isMutating)
        // Prevent subsequent requests from referring to responses before this request
        // (previous responses are considered stale after a mutation).
        Some(requestResponse.requestId)
      else
        state.lastMutatingRequestId

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

    val (deferred, processedOpts) =
      conditions.map {
        case (condition, Right(true -> predicate))  => condition -> Right(Some(Passed -> predicate))
        case (condition, Right(false -> predicate)) => condition -> Right(Some(Failed -> predicate))
        case (condition, Left(request))             => condition -> request.filterNot(_ => condition.isPrecondition).toLeft(None)
      }.toMap.partitionEither(identity)

    // Drop unresolvable conditions (i.e. preconditions that depend on unresolved requests, and thus will never appear).
    val processed =
      processedOpts.collect { case (key, Some(value)) => key -> value }

    val oldDeferred =
      if (endpoint.isMutating)
        // Clear previously deferred postconditions (mutations invalidate them).
        ScalaMap.empty[EndpointRequest, Set[ConditionIdWithState]]
      else
        state.deferredConditions

    val oldDeferredMinusSelf =
      oldDeferred - request

    val newDeferred =
      oldDeferredMinusSelf.merge(deferred.swap)

    val newHistory =
      if (endpoint.isMutating)
        // Truncate history (prevent it from growing indefinitely).
        // Histories are likely to stay very short, containing usually fewer than 100 requests: they are truncated on
        // each mutating request, leaving behind only the current request, and any requests depended on by deferred
        // conditions of the current request AND any requests depended on by prior request's conditions that depended
        // on the current request.
        if (newDeferred.nonEmpty) {
          val earliestDependency = newDeferred.values.flatten.map(_.earliestDependency).min
          history.dropWhile(_.requestId =!= earliestDependency)
        }
        else
          Chain.one(requestResponse)
      else
        history

    assert(
      newHistory.lastOption.exists(_.requestId === requestResponse.requestId),
      "Resulting history must always contain current request/response."
    )

    Result(
      processed.collect { case (id, state) => id.withoutState -> state },
      State(
        newDeferred,
        nextMutatingRequestId,
        newHistory
      )
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
