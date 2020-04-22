package testbot.runtime

import alleycats.std.all._
import cats.data.Chain
import cats.implicits._
import fs2.Stream
import io.circe.Json
import monix.eval.Task
import playground.EndpointRequestResponse
import testbot.common.FunctorExtensions._
import testbot.runtime.ConditionStatus.{Failed, Passed, ResolvedConditionStatus, Unresolvable}
import testbot.runtime.resolvers.BaseSymbolResolver
import testbot.schema.SymbolExtensions._
import testbot.schema.{ApplicationSchema, ConditionIdWithState}
import testbot.{SchemaSymbols => S}

import scala.collection.immutable.{Map => ScalaMap}

object ResponseValidator {
  /**
   * Streaming alternative of the online algorithm (this function encapsulates state management).
   */
  def stream[A](
    schema: ApplicationSchema,
    responseStream: Stream[Task, A])(
    f: A => EndpointRequestResponse
  ): Stream[Task, (A, ValidatedRequestResponse)] =
    responseStream
      .mapAccumulate(ResponseValidationState.initial) { (oldState, response) =>
        val ResponseValidationResult(result, newState) = validate(schema, oldState, f(response))
        (newState, (response, ValidatedRequestResponse(f(response), result)))
      }
      .map(_._2)

  /**
   * Online algorithm for validating server responses.
   */
  def validate(
    schema: ApplicationSchema,
    state: ResponseValidationState,
    requestResponse: EndpointRequestResponse
  ): ResponseValidationResult = {
    val history = state.history :+ requestResponse

    def getResponse(id: EndpointRequestId) =
      history.find(_.requestId === id).getOrElse(throw new Exception("History was truncated incorrectly."))

    def getResponseAfter(after: EndpointRequestId, matching: EndpointRequest) =
      history.findAfter(_.requestId === after, _.request === matching)

    def getResponseBefore(before: EndpointRequestId, upTo: Option[EndpointRequestId], matching: EndpointRequest) = {
      require(upTo.forall(_.value < before.value))
      history
        .reverse
        .findAfter(_.requestId === before, _.request === matching)
        .filter(r => upTo.forall(_.value <= r.requestId.value))
    }

    val endpoint      = schema.endpoint(requestResponse.request.endpointId)
    val ownConditions = endpoint.conditions.map {
      case (conditionId, predicate) =>
        val requestId             = requestResponse.requestId
        val lastMutatingRequestId = state.lastMutatingRequestId
        val reverseLookup         = getResponseBefore(requestId, lastMutatingRequestId, _)
        val earliestDependency    = findEarliestDependency(
          schema,
          reverseLookup,
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
    val allConditions      = ownConditions ++ deferredConditions.map(x => x -> schema.endpoint(x.conditionId.endpointId).conditions(x.conditionId)).toMap
    val noForwardLookup    = (_: EndpointRequest) => None: Option[EndpointRequestResponse]


    val nextMutatingRequestId =
      if (endpoint.isMutating)
        // Prevent subsequent requests from referring to responses before this request
        // (previous responses are considered stale after a mutation).
        Some(requestResponse.requestId)
      else
        state.lastMutatingRequestId

    val conditions =
      allConditions
        .groupBy { case (conditionId, _) => conditionId.provenance }
        .toList
        .flatMap { case (requestId, conditions) =>
          val response = getResponse(requestId)
          conditions.toList.map { case (condition, predicate) =>
            val isPrecondition = condition.isPrecondition
            val reverseLookup  = getResponseBefore(requestId, condition.lastMutatingRequestId, _)
            val forwardLookup  = if (isPrecondition) noForwardLookup else getResponseAfter(requestId, _)
            val resolution     = resolvePredicate(
              schema,
              reverseLookup,
              forwardLookup,
              response,
              predicate
            )
            (
              condition,
              resolution
            )
          }
        }

    val (deferred, processed) =
      conditions.map {
        case (condition, Right(true))   => condition -> Right(Passed)
        case (condition, Right(false))  => condition -> Right(Failed)
        case (condition, Left(request)) => condition -> request.filterNot(_ => condition.isPrecondition).toLeft(Unresolvable)
      }.toMap.partitionEither(identity)

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
      if (endpoint.isMutating) {
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
      }
      else
        history

    assert(
      newHistory.lastOption.exists(_.requestId === requestResponse.requestId),
      "Resulting history must always contain current request/response."
    )

    ResponseValidationResult(
      processed.collect { case (id, state: ResolvedConditionStatus) => id.withoutState -> state },
      ResponseValidationState(
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
    requestResponse: EndpointRequestResponse,
    predicate: S.Predicate
  ): Either[Option[EndpointRequest], Boolean] = {
    val convertSymbol     = convertToBaseSymbol(schema, getPastResponse, getFutureResponse, requestResponse, _)
    val basePredicate     = BaseSymbolResolver.convertToBasePredicate(S)(predicate)(convertSymbol)
    val resolvedPredicate = basePredicate.map(BaseSymbolResolver.resolvePredicate)
    resolvedPredicate
  }

  private def resolveSymbol(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => Option[EndpointRequestResponse],
    getFutureResponse: EndpointRequest => Option[EndpointRequestResponse],
    requestResponse: EndpointRequestResponse,
    symbol: S.Symbol
  ): Either[Option[EndpointRequest], Json] = {
    val convertSymbol  = convertToBaseSymbol(schema, getPastResponse, getFutureResponse, requestResponse, _)
    val baseSymbol     = BaseSymbolResolver.convertToBaseSymbol(S)(symbol)(convertSymbol)
    val resolvedSymbol = baseSymbol.map(BaseSymbolResolver.resolveSymbol)
    resolvedSymbol
  }

  private def findEarliestDependency(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => Option[EndpointRequestResponse],
    requestResponse: EndpointRequestResponse,
    predicate: S.Predicate
  ): EndpointRequestId = {
    val reverseLookups =
      predicate.toList.collect { case x: S.Endpoint if !x.evaluateAfterExecution => x }

    val reverseLookupResolutions =
      reverseLookups.map(resolveEndpointReference(schema, getPastResponse, _ => None, requestResponse, _))

    val reverseLookupResponses =
      reverseLookupResolutions.flatMap(_.toOption)

    (requestResponse.requestId :: reverseLookupResponses.map(_.requestId)).min
  }

  private def convertToBaseSymbol(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => Option[EndpointRequestResponse],
    getFutureResponse: EndpointRequest => Option[EndpointRequestResponse],
    requestResponse: EndpointRequestResponse,
    symbol: S.OwnSymbols
  ): Either[Option[EndpointRequest], Json] =
    symbol match {
      case S.ResponseBody    => requestResponse.response.body.asRight
      case S.StatusCode      => Json.fromInt(requestResponse.response.status).asRight
      case S.Parameter(name) => requestResponse.request.parameterValues(name).asRight
      case endpoint: S.Endpoint =>
        resolveEndpointReference(
          schema,
          getPastResponse,
          getFutureResponse,
          requestResponse,
          endpoint
        ).map(_.response.body)
    }

  private def resolveEndpointReference(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => Option[EndpointRequestResponse],
    getFutureResponse: EndpointRequest => Option[EndpointRequestResponse],
    requestResponse: EndpointRequestResponse,
    endpoint: S.Endpoint
  ): Either[Option[EndpointRequest], EndpointRequestResponse] = {
    val resolvedParametersF =
      endpoint.parameters.traverse(
        resolveSymbol(
          schema,
          getPastResponse,
          getFutureResponse,
          requestResponse,
          _
        )
      )

    // Do not defer condition if it's dependent on a request that was supposed to have happened in the past:
    val dependsOn = Some(_: EndpointRequest).filter(_ => endpoint.evaluateAfterExecution)

    val responseF = (request: EndpointRequest) => {
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
}
