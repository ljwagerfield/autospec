package spike.runtime

import cats.data.{Chain, EitherT, OptionT, State}
import cats.implicits._
import io.circe.Json
import playground.EndpointRequestResponse
import spike.RuntimeSymbols.Predicate._
import spike.RuntimeSymbols._
import spike.common.FunctorExtensions._
import spike.runtime.ConditionStatus.{Failed, Passed, ResolvedConditionStatus, Unresolvable}
import spike.runtime.SymbolConverter.SymbolConverterError.{UnresolvedForwardLookup, UnresolvedReverseLookup}
import spike.schema.{ApplicationSchema, ConditionId, ConditionIdWithState, Precondition}
import spike.{SchemaSymbols => S}

import scala.collection.immutable.{Map => ScalaMap}

object ResponseValidator {
  private type RequestIndex              = Int
  private type PostconditionsByRequest   = ScalaMap[RequestIndex, ScalaMap[ConditionId, Predicate]]
  private type EarliestRequestDependency = EndpointRequestId

  case class ResponseValidationState(
    deferredConditions: ScalaMap[EndpointRequest, Set[ConditionIdWithState]],
    lastMutatingRequestId: Option[EndpointRequestId],
    history: Chain[EndpointRequestResponse]
  )

  object ResponseValidationState {
    val initial = ResponseValidationState(ScalaMap.empty, None, Chain.empty)
  }

  case class ResponseValidationResult(
    completedConditions: ScalaMap[ConditionIdWithState, ConditionStatus],
    state: ResponseValidationState
  ) {
    def resolvedConditions: ScalaMap[ConditionIdWithState, ResolvedConditionStatus] =
      completedConditions.collect {
        case (id, status: ResolvedConditionStatus) => id -> status
      }
  }

  def validateConditions(
    schema: ApplicationSchema,
    state: ResponseValidationState,
    requestResponse: EndpointRequestResponse
  ): ResponseValidationResult = {
    val request       = requestResponse.request
    val endpoint      = schema.endpoint(requestResponse.request.endpointId)
    val ownConditions: ScalaMap[ConditionIdWithState, S.Predicate] = endpoint.conditions.map {
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
        case (condition, Left(request)) => condition -> (if (condition.isPrecondition) Right(Unresolvable) else Left(request))
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
  ): EitherT[State[EarliestRequestDependency, *], EndpointRequest, Boolean] = {

  }

  private def resolveSymbol(
    schema: ApplicationSchema,
    getPastResponse: EndpointRequest => OptionT[State[EarliestRequestDependency, *], EndpointResponse],
    getFutureResponse: EndpointRequest => Option[EndpointResponse],
    requestResponse: EndpointRequestResponse,
    predicate: S.Symbol
  ): EitherT[State[EarliestRequestDependency, *], EndpointRequest, Json] = {

  }









  private def generate(schema: ApplicationSchema, paths: List[TestPath]): TestPlan =
    TestPlan(
      schema,
      paths.map(addChecksToTestPath(schema, _))
    )

  private def addChecksToTestPath(schema: ApplicationSchema, testPath: TestPath): TestPathWithChecks =
    TestPathWithChecks(
      testPath.id,
      testPath
        .requests
        .foldLeft(StateOld(0, 0, Chain.empty, testPath.requests, Chain.empty, ScalaMap.empty))(addChecksToRequest(schema, _, _))
        .requests
    )

  private def addChecksToRequest(schema: ApplicationSchema, state: StateOld, request: EndpointRequest): StateOld = {
    val requestIndex       = state.currentRequestIndex
    val endpoint           = schema.endpoint(request.endpointId)
    val preconditionScope  = state.preconditionScope
    val postconditionScope = state.postconditionScope.tail

    val (_, preconditions) =
      endpoint.preconditionMap.toList.partitionBifold { case (conditionId, Precondition(predicate, expectedStatus)) =>
        for {
          success <- SymbolConverter.convertToRuntimePredicate(
            request,
            requestIndex,
            state.preconditionOffset,
            preconditionScope,
            Chain.empty,
            predicate
          )
          successOrExpectedError =
          Or(
            success,
            Equals(
              StatusCode(requestIndex),
              Literal(Json.fromInt(expectedStatus))
            )
          )
        } yield (
          conditionId,
          successOrExpectedError
        )
      }

    val (unresolvedPostconditions, postconditions) =
      endpoint.postconditionMap.toList.flatPartitionBifold { case (conditionId, predicate) =>
        {for {
          success <- SymbolConverter.convertToRuntimePredicate(
            request,
            requestIndex,
            state.preconditionOffset,
            preconditionScope,
            postconditionScope,
            predicate
          )
        } yield (
          conditionId,
          success
        )}.leftMap(_.toList).map(_ :: Nil)
      }

    val postconditionsByRequest =
      postconditions.toMap.groupBy { case (_, predicate) => maxRequestIndex(predicate).getOrElse(requestIndex) }

    val hasUnresolvedForwardLookups =
      unresolvedPostconditions.exists {
        case UnresolvedForwardLookup => true
        case UnresolvedReverseLookup => false
      }

    // If any postconditions reference requests other than the current one, then by default we assume this is a mutating
    // request. It may not be: for example, a pure endpoint can reference other pure endpoints to declare synchronicity
    // with them. In these scenarios, the developer must explicitly mark the endpoint as 'pure'.
    val isMutation =
      !endpoint.forcePure && (
        hasUnresolvedForwardLookups || postconditionsByRequest.keySet.maxOption.exists(_ > requestIndex)
      )

    val (previousPostconditions, nextPreconditionScope) =
      if (isMutation)
        (
          // Clear all previously deferred postconditions (mutations invalidate them).
          ScalaMap(
            requestIndex -> state.deferredPostconditions.getOrElse(requestIndex, ScalaMap.empty)
          ),
          // Prevent references to previously executed requests (their results are considered stale after a mutation).
          Chain(
            request
          )
        )
      else
        (state.deferredPostconditions, state.preconditionScope :+ request)

    val preconditionScopeShrinkage = (state.preconditionScope.size - (nextPreconditionScope.size - 1)).toInt

    val mergedPostconditions       = mergePostconditions(previousPostconditions, postconditionsByRequest)
    val immediatePostconditions    = mergedPostconditions.getOrElse(requestIndex, ScalaMap.empty)
    val deferredPostconditions     = mergedPostconditions - requestIndex

    val requestWithChecks = EndpointRequestWithChecks(
      request,
      preconditions.toMap ++ immediatePostconditions
    )

    StateOld(
      currentRequestIndex    = requestIndex + 1,
      preconditionOffset     = state.preconditionOffset + preconditionScopeShrinkage,
      preconditionScope      = nextPreconditionScope,
      postconditionScope     = postconditionScope,
      requests               = state.requests :+ requestWithChecks,
      deferredPostconditions = deferredPostconditions
    )
  }

  private def maxRequestIndex(symbol: Symbol): Option[RequestIndex] =
    symbol match {
      // Leafs: Request Index
      case ResponseBody(requestIndex) => Some(requestIndex)
      case StatusCode(requestIndex)   => Some(requestIndex)

      // Leafs: N/A
      case Literal(_)                 => None
      case LambdaParameter(_)         => None

      // Recursive Data Structures
      case Map(symbol, path)          => max(maxRequestIndex(symbol), maxRequestIndex(path))
      case FlatMap(symbol, path)      => max(maxRequestIndex(symbol), maxRequestIndex(path))
      case Flatten(symbol)            => maxRequestIndex(symbol)
      case Find(symbol, predicate)    => max(maxRequestIndex(symbol), maxRequestIndex(predicate))
      case Count(symbol)              => maxRequestIndex(symbol)
      case Distinct(symbol)           => maxRequestIndex(symbol)
      case Prepend(item, collection)  => max(maxRequestIndex(item), maxRequestIndex(collection))
      case Append(collection, item)   => max(maxRequestIndex(item), maxRequestIndex(collection))
      case Concat(left, right)        => max(maxRequestIndex(left), maxRequestIndex(right))
      case Equals(left, right)        => max(maxRequestIndex(left), maxRequestIndex(right))
      case And(left, right)           => max(maxRequestIndex(left), maxRequestIndex(right))
      case Or(left, right)            => max(maxRequestIndex(left), maxRequestIndex(right))
      case Not(predicate)             => maxRequestIndex(predicate)
      case Exists(symbol, predicate)  => max(maxRequestIndex(symbol), maxRequestIndex(predicate))
      case Contains(collection, item) => max(maxRequestIndex(collection), maxRequestIndex(item))
    }

  private def max(a: Option[RequestIndex], b: Option[RequestIndex]): Option[RequestIndex] =
    (a, b).mapN(Math.max).orElse(a).orElse(b)

  private def mergePostconditions(a: PostconditionsByRequest, b: PostconditionsByRequest): PostconditionsByRequest =
    (a.toList ::: b.toList).groupMapReduce(_._1)(_._2)(_ ++ _)

  private case class StateOld(
    currentRequestIndex: Int,
    preconditionOffset: Int,
    preconditionScope: Chain[EndpointRequest],
    postconditionScope: Chain[EndpointRequest],
    requests: Chain[EndpointRequestWithChecks],
    deferredPostconditions: PostconditionsByRequest
  )

  private object StateOld {
    val initial = StateOld(0, 0, Chain.empty, Chain.empty, Chain.empty, ScalaMap.empty)
  }
}