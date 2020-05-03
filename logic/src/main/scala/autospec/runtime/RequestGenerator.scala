package autospec.runtime

import alleycats.std.all._
import autospec.common.FunctorExtensions._
import autospec.common.MathUtils._
import autospec.common.StringExtensions._
import autospec.runtime.RequestGenerator.{RequestCandidate, WithState, _}
import autospec.runtime.exceptions.EndpointRequestFailure
import autospec.runtime.resolvers.{BaseSymbolResolver, IntermediateSymbolResolver, SymbolConverter}
import autospec.schema._
import autospec.{GlobalSchemaSymbols => SG, IntermediateSymbols => I, LocalSchemaSymbols => SL}
import cats.data.{NonEmptyList, OptionT, StateT, ValidatedNel}
import cats.implicits._
import cats.{~>, Id}
import fs2.Stream
import io.circe.Json
import monix.eval.Task
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.immutable.Queue
import scala.util.Random

/**
  * Generates the next request to send to the REST API under test.
  *
  * This involves outputting a meaningful sequence of requests that are most likely to uncover bugs in the REST API, and
  * doing so in as few requests as possible (this is the most challenging part of AutoSpec, and always needs improving).
  */
class RequestGenerator(requestExecutor: EndpointRequestExecutor) {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def stream(session: Session): Stream[Task, Either[EndpointRequestFailure, EndpointRequestResponse]] = {
    type F[A] = OptionT[Task, A]
    val fromOption = λ[Option ~> F](OptionT.fromOption[Task](_))
    val fromTask   = λ[Task ~> F](OptionT.liftF(_))

    Stream.unfoldEval(initialState) { state =>
      nextRequest(session.schema)
        .mapK(fromOption)
        .flatMap { request =>
          StateT
            .liftF(
              requestExecutor
                .execute(session, request)
                .value
            )
            .mapK(fromTask)
        }
        .flatTap(processResponse(_).mapK(fromOption))
        .run(state)
        .map(_.swap)
        .value
    }
  }

  private def nextRequest(schema: ApplicationSchema): WithState[EndpointRequest] =
    for {
      callableEndpoints <- getCallableEndpoints(schema)
      nextRequest       <- nextRequestFromState.orElse(nextRequestRandom(callableEndpoints))
      opportunities      = Opportunities(callableEndpoints.map(_.endpointId).toSet)
      _                 <- saveOpportunities(opportunities)
    } yield nextRequest

  private def nextRequestFromState: WithState[EndpointRequest] =
    StateT { state =>
      val nextRequests = unresolvedRequestsFromPostconditions(state)
      val (reverse, forward) = nextRequests.partitionMap {
        case (request, SG.Endpoint(_, _, false)) => Left(request)
        case (request, SG.Endpoint(_, _, true))  => Right(request)
      }
      val nextReverseRequest = reverse.headOption.map(state -> _)
      val requestUnderTest   = state.popRequestUnderTest
      val nextForwardRequest = forward.headOption.map(state -> _)

      // Order is important.
      // Also, we call these regardless of if they appear in 'callableEndpoints', as their availability is informed by
      // postconditions on endpoints, rather than preconditions on endpoints that's used for 'callableEndpoints'.  E.g.
      // 'createFoo' has 'getFoo(x)' as a postcondition, but 'getFoo(x)' would never be discovered by itself. But by
      // calling 'getFoo(x)' we now have a range of values available for preconditions dependent on 'getFoo'.
      nextReverseRequest
        .orElse(requestUnderTest)
        .orElse(nextForwardRequest)
    }

  private def nextRequestRandom(callableEndpoints: List[EndpointCandidate]): WithState[EndpointRequest] = {
    val maybeSaveNextRequestToState =
      StateT.modifyF[Option, State] { state =>
        val chosenEndpointOpt = weightedRandom(callableEndpoints)(x => endpointWeight(state, x.endpointId))
        chosenEndpointOpt.map { chosenEndpoint =>
          val chosenRequest =
            chosenEndpoint
              .possibleRequests
              .toList(
                // TODO: bias these based on how frequently available they are... but don't favour endpoints with more paramaters any more than those with equal! Currently we're just randomly selecting.
                Random.nextInt(chosenEndpoint.possibleRequests.size)
              )

          state.setupRequestUnderTest(chosenRequest)
        }
      }

    maybeSaveNextRequestToState *> log("Started new random sequence of requests") *> nextRequestFromState
  }

  private def getCallableEndpoints(schema: ApplicationSchema): WithState[List[EndpointCandidate]] =
    StateT.inspect(state => schema.endpoints.flatMap(getCallableEndpoint(_, state)))

  private def getCallableEndpoint(endpoint: EndpointDefinition, state: State): Option[EndpointCandidate] =
    getRequestCandidates(endpoint, state).toNel.map(
      EndpointCandidate(endpoint.id, _)
    )

  private def getRequestCandidates(endpoint: EndpointDefinition, state: State): List[RequestCandidate] =
    generateParamCombinations(endpoint, state).flatMap {
      case (paramCombinations, partiallyResolvedPredicates) =>
        val paramCombinationsThatSatisfyPredicates =
          paramCombinations.filter { params =>
            partiallyResolvedPredicates.forall(predicate => resolvePredicate(predicate, params.toMap))
          }

        paramCombinationsThatSatisfyPredicates.map { params =>
          val currentRequest = EndpointRequest(endpoint.id, params.toMap)
          val postconditions = getPostconditions(endpoint, currentRequest)
          RequestCandidate(
            currentRequest,
            postconditions
          )
        }
    }

  private def generateParamCombinations(
    endpoint: EndpointDefinition,
    state: State
  ): List[(NonEmptyList[List[(EndpointParameterName, Json)]], List[I.Predicate])] =
    generatePartialParamCombinationsFromEndpointReferences(endpoint, state).run(Map.empty).map {
      case (partialParamCombination, partiallyResolvedPredicates) =>
        val resolvedParamNames = partialParamCombination.keySet
        val unresolvedParams   = endpoint.parameters.filterNot(x => resolvedParamNames.contains(x.name))

        // Each sublist represents the set of all possible values for a single parameter name.
        // For endpoint params, there is only one value per param, as the outer map to this comment is already mapping
        // over each endpoint param combination individually (since the partially resolved predicates are unique per
        // endpoint param combination).
        val paramsFromEndpoints = partialParamCombination.toList.map(_ :: Nil)
        val paramsFromRandom = unresolvedParams
          .map { parameter =>
            getParameterRange(endpoint.id, parameter.`type`).map(parameterValue => parameter.name -> parameterValue)
          }

        val paramCombinations =
          cartesianProduct(
            paramsFromEndpoints ::: paramsFromRandom
          )

        val paramCombinationsOrNoParams =
          NonEmptyList
            .fromList(paramCombinations)
            .getOrElse(NonEmptyList.one(Nil))

        paramCombinationsOrNoParams -> partiallyResolvedPredicates
    }

  private def generatePartialParamCombinationsFromEndpointReferences(
    endpoint: EndpointDefinition,
    state: State
  ): WithParamResolutionState[List[I.Predicate]] =
    endpoint
      .preconditions
      .map(_.predicate)
      .traverse(resolveEndpointsInPredicate(_, state))

  private def resolvePredicate(predicate: I.Predicate, resolvedParams: Map[EndpointParameterName, Json]): Boolean =
    IntermediateSymbolResolver.resolvePredicate(predicate, resolvedParams)

  private def resolveEndpointsInPredicate(
    predicate: SL.Predicate,
    state: State
  ): WithParamResolutionState[I.Predicate] =
    SymbolConverter.convertPredicate(SL, I)(predicate)(convertSchemaSymbols(_, state))

  private def convertSchemaSymbols(
    symbol: SL.OwnSymbols,
    state: State
  ): WithParamResolutionState[I.Symbol] =
    symbol match {
      case endpoint: SL.Endpoint => resolveEndpointSymbol(endpoint, state).widen
      case SL.Parameter(name)    => (I.Parameter(name): I.Symbol).pure[WithParamResolutionState]
      case SL.ResponseBody | SL.StatusCode =>
        throw new Exception(s"Preconditions cannot contain a ${symbol.getClass.getSimpleName}")
    }

  private def resolveEndpointSymbol(
    endpoint: SL.Endpoint,
    state: State
  ): WithParamResolutionState[I.Literal] = {
    val previousRequests = state.responses.getOrElse(endpoint.endpointId, List.empty)
    val paramCombinations =
      endpoint.parameters.traverse {
        case x: SL.Endpoint  => resolveEndpointSymbol(x, state).map(_.value.asRight[EndpointParameterName])
        case x: SL.Parameter => x.name.asLeft[Json].pure[WithParamResolutionState]
        case x: SL.Literal   => x.value.asRight[EndpointParameterName].pure[WithParamResolutionState]
        case x =>
          throw new UnsupportedOperationException(
            s"""
              |Endpoint references inside preconditions must currently use
              |either literals or other endpoint references as parameters.
              |The use of ${x.getClass.getSimpleName} is currently unsupported.
              |""".stripMarginAndLineBreaks
          )
      }

    paramCombinations.flatMap(matchRequestsByParams(previousRequests, _))
  }

  private def matchRequestsByParams(
    requests: Iterable[EndpointRequestResponse],
    params: Map[EndpointParameterName, Either[EndpointParameterName, Json]]
  ): WithParamResolutionState[I.Literal] = {
    val (variableParams, literalParams) = params.partitionEither(identity)
    val endpointParamsByVariable        = variableParams.swap.toList

    StateT { (resolvedParams: Map[EndpointParameterName, Json]) =>
      matchRequestsByLiterals(requests, literalParams).map(resolvedParams -> _).toList
    }.flatMap(
      matchRequestIfVariablesFit(_, endpointParamsByVariable).mapK(λ[Option ~> List](_.toList))
    )
  }

  private def matchRequestsByLiterals(
    requests: Iterable[EndpointRequestResponse],
    partialParams: Map[EndpointParameterName, Json]
  ): Iterable[EndpointRequestResponse] =
    requests.filter { request =>
      partialParams.forall {
        case (paramName, paramValue) =>
          request.request.parameterValues(paramName) === paramValue
      }
    }

  private def matchRequestIfVariablesFit(
    request: EndpointRequestResponse,
    endpointParamsByVariable: List[(EndpointParameterName, Set[EndpointParameterName])]
  ): StateT[Option, Map[EndpointParameterName, Json], I.Literal] =
    endpointParamsByVariable.traverse_ {
      case (variableParam, endpointParams) =>
        StateT { (resolvedParams: Map[EndpointParameterName, Json]) =>
          endpointParams
            .map(request.request.parameterValues.apply)
            .toList
            .one // If same variable used for multiple parameters for the endpoint call, like foo(x,x), and the resolved values differ, elide the request.
            .map(paramValue => (resolvedParams ++ Map(variableParam -> paramValue), ()))
        }
    }.as(I.Literal(request.response.body))

  private def saveOpportunities(opportunities: Opportunities): WithState[Unit] =
    StateT.modify(_.appendOpportunities(opportunities))

  private def processResponse(response: Either[EndpointRequestFailure, EndpointRequestResponse]): WithState[Unit] =
    saveResponse(response)

  private def saveResponse(response: Either[EndpointRequestFailure, EndpointRequestResponse]): WithState[Unit] = {
    val updateState = StateT.modify[Option, State](_.appendResponse(response))
    val logMessage  = log(s"Stored ${response.fold(_ => "failed", _ => "successful")} response")
    updateState *> logMessage
  }

  private def getPostconditions(endpoint: EndpointDefinition, request: EndpointRequest): List[SG.Predicate] =
    endpoint.postconditions.map(predicate =>
      SymbolConverter.convertPredicate(SL, SG)(predicate)(convertOwnLocalSymbolToGlobalSymbol(_, request))
    )

  private def convertLocalSymbolToGlobalSymbol(symbol: SL.Symbol, request: EndpointRequest): Id[SG.Symbol] =
    SymbolConverter.convertSymbol(SL, SG)(symbol)(convertOwnLocalSymbolToGlobalSymbol(_, request))

  private def convertOwnLocalSymbolToGlobalSymbol(
    symbol: SL.OwnSymbols,
    request: EndpointRequest
  ): Id[SG.Symbol] =
    symbol match {
      case SL.Parameter(name) => SG.Literal(request.parameterValues(name))
      case SL.ResponseBody =>
        SG.Endpoint(
          request.endpointId,
          request.parameterValues.view.mapValues(SG.Literal.apply).toMap,
          evaluateAfterExecution = true
        )
      case SL.StatusCode =>
        // Todo: replace with status code reference (see: https://trello.com/c/m7gs0JnN/45-allow-sendpoint-to-dereference-status-codes-too)
        SG.Endpoint(
          request.endpointId,
          request.parameterValues.view.mapValues(SG.Literal.apply).toMap,
          evaluateAfterExecution = true
        )
      case SL.Endpoint(endpointId, parameters, evaluateAfterExecution) =>
        SG.Endpoint(
          endpointId,
          parameters.traverse(convertLocalSymbolToGlobalSymbol(_, request)),
          evaluateAfterExecution
        )
    }

  private def unresolvedRequestsFromPostconditions(state: State): List[(EndpointRequest, SG.Endpoint)] =
    unresolvedRequestsFromPredicates(
      state.unresolvedPostconditions,
      state.postconditionReverseLookupScope,
      state.postconditionForwardLookupScope
    )

  private def unresolvedRequestsFromPredicates(
    predicates: Seq[SG.Predicate],
    reverseLookupScope: Map[EndpointRequest, EndpointResponse],
    forwardLookupScope: Map[EndpointRequest, EndpointResponse]
  ): List[(EndpointRequest, SG.Endpoint)] =
    predicates.flatMap(predicate =>
      BaseSymbolResolver.convertToBasePredicate(SG)(predicate)(
        tryResolveRequestFromEndpointSymbol(_, reverseLookupScope, forwardLookupScope)
      ).fold(_.toList, _ => Nil)
    ).toList

  private def tryResolveRequestFromEndpointSymbol(
    symbol: SG.OwnSymbols,
    reverseLookupScope: Map[EndpointRequest, EndpointResponse],
    forwardLookupScope: Map[EndpointRequest, EndpointResponse]
  ): ValidatedNel[(EndpointRequest, SG.Endpoint), Json] =
    symbol match {
      case endpoint @ SG.Endpoint(endpointId, parameters, evaluateAfterExecution) =>
        parameters.traverse(tryResolveSymbolFromRequestScopes(_, reverseLookupScope, forwardLookupScope)).andThen {
          parameterValues =>
            val request =
              EndpointRequest(
                endpointId,
                parameterValues
              )

            val scope =
              if (evaluateAfterExecution)
                forwardLookupScope
              else
                reverseLookupScope

            scope.get(request).map(_.body).toValidNel(request -> endpoint)
        }
    }

  private def tryResolveSymbolFromRequestScopes(
    symbol: SG.Symbol,
    reverseLookupScope: Map[EndpointRequest, EndpointResponse],
    forwardLookupScope: Map[EndpointRequest, EndpointResponse]
  ): ValidatedNel[(EndpointRequest, SG.Endpoint), Json] =
    BaseSymbolResolver.convertToBaseSymbol(SG)(symbol)(
      tryResolveRequestFromEndpointSymbol(_, reverseLookupScope, forwardLookupScope)
    ).map(
      BaseSymbolResolver.resolveSymbol
    )

  // Returns a 'meaningfully sampled set' of all possible values this parameter can be. For boolean this is easy:
  // always return true and false. However, all other types have a much larger space, so we have to return a sample
  // that ideally represents meaningful boundaries.
  private def getParameterRange(endpointId: EndpointId, parameterType: EndpointParameterType): List[Json] = {
    import EndpointParameterType._
    // Since we're checking historical availability of these parameters, we need to use the same set of parameters,
    // and not completely random ones. This is because each 'possible request' that gets generated in this round is
    // remembered for the next round as 'the requests that were callable', and anything not in that set is assumed
    // to have not been possible in that round. So if we generate a new list of 'possible requests' with unique variables
    // in the next round, they won't exist in the previous round, and they will be seen as 'only just available in this round'
    // and will be weighted high. Instead, we need to use the same random variables in each round. HOWEVER, to avoid
    // unintentional bleeding across endpoints, we make the params unique to each endpoint by using the endpoint ID as
    // a salt, just to prevent any confusion that we might be talking about common business objects across the endpoints
    // (because we are not).
    // ----
    // Todo: we need to lookup the thing referencing this variable to find a better domain. E.g. in all the following
    // cases, we should be able to identify the exact domain of this parameter:
    // requires(['draft', 'inviting', 'amending'].contains(state))
    // or
    // requires(state == 'draft' || state == 'inviting')
    // or
    // requires(getPreset(x).docId == y)
    val saltString = endpointId.value
    parameterType match {
      case Boolean => List(true, false).map(Json.fromBoolean)
      case String  => List("", saltString, s"$saltString-a", s"$saltString-b", s"$saltString-c").map(Json.fromString)
      case Int16   => List(-3, -2, -1, 0, 1, 2, 3, Int16.minVal, Int16.maxVal).map(Json.fromInt)
      case Int32   => List(-3, -2, -1, 0, 1, 2, 3, Int.MinValue, Int.MaxValue).map(Json.fromInt)
      case Int64   => List(-3, -2, -1, 0, 1, 2, 3, Long.MinValue, Long.MaxValue).map(Json.fromLong)
      case Single =>
        List(-3, -2, -1, -0.1F, 0, 0.1F, 1, 2, 3, Float.MinValue, Float.MaxValue).map(x => Json.fromFloat(x).get)
      case Double =>
        List(-3, -2, -1, -0.1D, 0, 0.1D, 1, 2, 3, scala.Double.MinValue, scala.Double.MaxValue).map(x =>
          Json.fromDouble(x).get
        )
      case Object(fields) =>
        cartesianProduct(
          fields.view.mapValues(x => getParameterRange(endpointId, x)).toMap
        )
          .map(Json.fromFields)
      case Array(elementTypes) =>
        val values =
          NonEmptyList
            .fromList(elementTypes)
            .getOrElse(NonEmptyList.of(Boolean, String, Int32))
            .toList
            .flatMap(getParameterRange(endpointId, _))

        val padding = 3 - values.size

        val valuesPadded =
          if (padding > 0)
            List.fill(padding)(values.head) ::: values
          else
            values

        List(List.empty, valuesPadded.take(1), valuesPadded.take(2), valuesPadded).map(Json.fromValues)
    }
  }

  private def endpointWeight(state: State, endpoint: EndpointId): Int = {
    val count      = state.opportunities.count(_.possibleRequests.contains(endpoint))
    val maxPenalty = maxBiasFactor + 1

    // Random (no biasing of less-frequently available endpoints)
    //         Empirical performance (lower is better): 20,000 requests to find all unique 3-strands in a sample API.
    // 1

    // Relative (endpoint A that has appeared Nx less than another endpoint B will have Nx more chance
    //           of being called in this iteration than endpoint B).
    //           Empirical performance (lower is better): 10,000 requests to find all unique 3-strands in a sample API.
    // val penalty    = count + 1
    // Math.round((1D / (penalty.toDouble / maxPenalty.toDouble)) * 100).toInt

    // Linear (least-frequent endpoint has Nx more chance of being called than most frequent endpoint,
    //         but only N/(N-1)x chance than second-least-frequent endpoint).
    //         Empirical performance (lower is better): 5,000 requests to find all unique 3-strands in a sample API.
    maxPenalty - count
  }

  private def log(message: String): WithState[Unit] =
    if (logger.isDebugEnabled)
      StateT.inspect { state =>
        def summariseResponses(value: Map[EndpointRequest, EndpointResponse]) =
          value.view.mapValues(_.status).toMap
        logger.debug(
          s"""
             |$message |
             |pendingRequestUnderTest=${state.pendingRequestUnderTest} |
             |unresolvedPostconditions=${state.unresolvedPostconditions} |
             |postconditionReverseLookupScope=${summariseResponses(state.postconditionReverseLookupScope)} |
             |postconditionForwardLookupScope=${summariseResponses(state.postconditionForwardLookupScope)} |
             |""".stripMarginAndLineBreaks
        )
      }
    else
      ().pure[WithState]

}

object RequestGenerator {

  /**
    * The maximum factor to use for biasing the selection of an infrequently-seen endpoint vs a frequently-seen endpoint.
    * For example, a factor of 10 says "if we've seen endpoint A once, and endpoint B every time, then give endpoint
    * A 10x the chance of being called than endpoint B".
    */
  private val maxBiasFactor = 10

  /**
    * For now we're assuming mean request/response sizes are under 1MB (1GB total).
    *
    * This limit is one of the reasons why it's important to keep test sequences focused on one area of the REST API at
    * a time, else the relevant history vanishes and requests are just generated randomly, due to lack of any deep
    * relevant heuristics.
    */
  private val maxHistory = 1000

  private val initialState = State(Queue.empty, Queue.empty, None, Nil, Map.empty, Map.empty)

  private type WithState[A]                = StateT[Option, State, A]
  private type WithParamResolutionState[A] = StateT[List, Map[EndpointParameterName, Json], A]

  private case class EndpointCandidate(endpointId: EndpointId, possibleRequests: NonEmptyList[RequestCandidate])

  private case class RequestCandidate(
    request: EndpointRequest,
    postconditions: List[SG.Predicate]
  )

  private case class Opportunities(possibleRequests: Set[EndpointId])

  private case class State(
    history: Queue[EndpointRequestResponse],
    opportunities: Queue[Opportunities],
    pendingRequestUnderTest: Option[EndpointRequest],
    unresolvedPostconditions: List[SG.Predicate],
    postconditionReverseLookupScope: Map[EndpointRequest, EndpointResponse],
    postconditionForwardLookupScope: Map[EndpointRequest, EndpointResponse]
  ) {

    lazy val historyMap: Map[EndpointRequest, EndpointRequestResponse] =
      // IMPORTANT: take latest response from each unique request.
      history.groupMapReduce(_.request)(identity)((_, last) => last)

    lazy val responses: Map[EndpointId, Iterable[EndpointRequestResponse]] = historyMap.groupMap(_._1.endpointId)(_._2)

    def setupRequestUnderTest(request: RequestCandidate): State =
      copy(
        pendingRequestUnderTest         = Some(request.request),
        unresolvedPostconditions        = request.postconditions,
        postconditionReverseLookupScope = Map.empty,
        postconditionForwardLookupScope = Map.empty
      )

    def popRequestUnderTest: Option[(State, EndpointRequest)] =
      pendingRequestUnderTest.map(r => copy(pendingRequestUnderTest = None) -> r)

    def appendOpportunities(newOpportunities: Opportunities): State =
      copy(
        opportunities = boundedAppend(opportunities, newOpportunities, maxBiasFactor)
      )

    def appendResponse(response: Either[EndpointRequestFailure, EndpointRequestResponse]): State = {
      // Until the request under test is executed, all requests we execute are for the reverse lookup scope.
      val isReverseLookupScope     = pendingRequestUnderTest.isDefined
      val hasPendingPostconditions = unresolvedPostconditions.nonEmpty

      def appendToScopeMaybe(
        scope: Map[EndpointRequest, EndpointResponse],
        isActiveScope: Boolean
      ): Map[EndpointRequest, EndpointResponse] =
        response.fold(
          _ => Map.empty,
          response =>
            if (hasPendingPostconditions && isActiveScope)
              scope ++ Map(response.request -> response.response)
            else
              scope
        )

      // On error, stop attempting to execute the request under test and its postconditions, as if we keep attempting
      // to execute the same request sequence over-and-over, we will get stuck in a failing cycle if the API has a
      // problem, so allow the request generator to randomly generate another request instead.
      copy(
        history                         = response.toOption.fold(history)(boundedAppend(history, _, maxHistory)),
        pendingRequestUnderTest         = response.fold(_ => None, _ => pendingRequestUnderTest),
        unresolvedPostconditions        = response.fold(_ => Nil, _ => unresolvedPostconditions),
        postconditionReverseLookupScope = appendToScopeMaybe(postconditionReverseLookupScope, isReverseLookupScope),
        postconditionForwardLookupScope = appendToScopeMaybe(postconditionForwardLookupScope, !isReverseLookupScope)
      )
    }

    private def boundedAppend[A](queue: Queue[A], item: A, maxSize: Int): Queue[A] =
      if (queue.size === maxSize)
        queue.enqueue(item).dequeue._2
      else
        queue.enqueue(item)

  }

}
