package spike.runtime

import alleycats.std.all._
import cats.data.{Chain, EitherT, OptionT, State}
import cats.implicits._
import io.circe.Json
import playground.EndpointRequestResponse
import spike.common.FunctorExtensions._
import spike.runtime.ConditionStatus.{Failed, Passed, Unresolvable}
import spike.runtime.resolvers.BaseSymbolResolver
import spike.schema.{ApplicationSchema, ConditionIdWithState}
import spike.{SchemaSymbols => S}

import scala.collection.immutable.{Map => ScalaMap}

object ResponseValidator {
  private type EarliestRequestDependency = EndpointRequestId

  def validateConditions(
    schema: ApplicationSchema,
    state: ResponseValidationState,
    requestResponse: EndpointRequestResponse
  ): ResponseValidationResult = {
    val request       = requestResponse.request
    val endpoint      = schema.endpoint(requestResponse.request.endpointId)
    val ownConditions = endpoint.conditions.map {
      case (conditionId, predicate) =>
        (
          ConditionIdWithState(
            conditionId,
            requestResponse.requestId,
            requestResponse.requestId,
            state.lastMutatingRequestId
          ),
          predicate
        )
    }
    val deferredConditions = state.deferredConditions.getOrElse(request, Set.empty).toList
    val allConditions      = ownConditions ++ deferredConditions.map(x => x -> schema.endpoint(x.conditionId.endpointId).conditions(x.conditionId)).toMap
    val noForwardLookup    = (_: EndpointRequest) => None: Option[EndpointResponse]
    val history            = state.history :+ requestResponse

    def getResponse(id: EndpointRequestId) =
      history.find(_.requestId === id).getOrElse(throw new Exception("History was truncated incorrectly."))

    def getResponseAfter(after: EndpointRequestId, matching: EndpointRequest) =
      history.findAfter(_.requestId === after, _.request === matching).map(_.response)

    def getResponseBefore(before: EndpointRequestId, upTo: Option[EndpointRequestId], matching: EndpointRequest) = {
      require(upTo.forall(_.value < before.value))
      OptionT(
        State { (min: EarliestRequestDependency) =>
          val dependency = history
            .reverse
            .findAfter(_.requestId === before, _.request === matching)
            .filter(r => upTo.forall(_.value <= r.requestId.value))
          val response  = dependency.map(_.response)
          val newMinOpt = dependency.map(_.requestId).filter(_.value < min.value)
          val newMin    = newMinOpt.getOrElse(min)
          newMin -> response
        }
      )
    }

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
            val (earliestDependency, resolution) = resolvePredicate(
              schema,
              reverseLookup,
              forwardLookup,
              response,
              predicate
            ).value.run(condition.earliestDependency).value
            (
              condition.copy(earliestDependency = earliestDependency),
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
        // Histories are likely to stay very short, containing usually fewer than 10 requests: they are truncated on
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
      processed,
      ResponseValidationState(
        newDeferred,
        nextMutatingRequestId,
        newHistory
      )
    )
  }

  private def resolvePredicate(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => OptionT[State[EarliestRequestDependency, *], EndpointResponse],
    getFutureResponse: EndpointRequest => Option[EndpointResponse],
    requestResponse: EndpointRequestResponse,
    predicate: S.Predicate
  ): EitherT[State[EarliestRequestDependency, *], Option[EndpointRequest], Boolean] = {
    val convertSymbol     = convertToBaseSymbol(schema, getPastResponse, getFutureResponse, requestResponse, _)
    val basePredicate     = BaseSymbolResolver.convertToBasePredicate(S)(predicate)(convertSymbol)
    val resolvedPredicate = basePredicate.map(BaseSymbolResolver.resolvePredicate)
    resolvedPredicate
  }

  private def resolveSymbol(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => OptionT[State[EarliestRequestDependency, *], EndpointResponse],
    getFutureResponse: EndpointRequest => Option[EndpointResponse],
    requestResponse: EndpointRequestResponse,
    symbol: S.Symbol
  ): EitherT[State[EarliestRequestDependency, *], Option[EndpointRequest], Json] = {
    val convertSymbol  = convertToBaseSymbol(schema, getPastResponse, getFutureResponse, requestResponse, _)
    val baseSymbol     = BaseSymbolResolver.convertToBaseSymbol(S)(symbol)(convertSymbol)
    val resolvedSymbol = baseSymbol.map(BaseSymbolResolver.resolveSymbol)
    resolvedSymbol
  }

  private def convertToBaseSymbol(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => OptionT[State[EarliestRequestDependency, *], EndpointResponse],
    getFutureResponse: EndpointRequest => Option[EndpointResponse],
    requestResponse: EndpointRequestResponse,
    symbol: S.OwnSymbols
  ): EitherT[State[EarliestRequestDependency, *], Option[EndpointRequest], Json] = {
    type F[A] = EitherT[State[EarliestRequestDependency, *], Option[EndpointRequest], A]
    symbol match {
      case S.ResponseBody    => requestResponse.response.body.pure[F]
      case S.StatusCode      => Json.fromInt(requestResponse.response.status).pure[F]
      case S.Parameter(name) => requestResponse.request.parameterValues(name).pure[F]
      case S.Endpoint(endpointId, parameters, evaluateAfterExecution) =>
        val resolvedParametersF =
          parameters.traverse(
            resolveSymbol(
              schema,
              getPastResponse,
              getFutureResponse,
              requestResponse,
              _
            )
          )

        // Do not defer condition if it's dependent on a request that was supposed to have happened in the past:
        val dependsOn = Some(_: EndpointRequest).filter(_ => evaluateAfterExecution)

        val responseF = (request: EndpointRequest) => {
          if (evaluateAfterExecution)
            OptionT.fromOption[State[EarliestRequestDependency, *]](
              getFutureResponse(request)
            )
          else
            getPastResponse(request)
        }.toRight(dependsOn(request))

        for {
          resolvedParameters <- resolvedParametersF
          request             = EndpointRequest(endpointId, resolvedParameters)
          response           <- responseF(request)
        } yield {
          response.body
        }
    }
  }
}