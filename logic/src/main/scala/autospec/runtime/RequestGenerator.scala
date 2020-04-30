package autospec.runtime

import autospec.common.StringExtensions._
import autospec.common.FunctorExtensions._
import autospec.common.MathUtils._
import autospec.runtime.RequestGenerator._
import autospec.runtime.exceptions.EndpointRequestFailure
import autospec.runtime.resolvers.{IntermediateSymbolResolver, SymbolConverter}
import autospec.schema._
import autospec.{IntermediateSymbols => I, SchemaSymbols => S}
import cats.data.{NonEmptyList, StateT}
import cats.implicits._
import alleycats.std.all._
import cats.~>
import io.circe.Json
import monix.eval.Task
import fs2.Stream

import scala.collection.immutable.Queue
import scala.util.Random

/**
  * Generates the next request to send to the REST API under test.
  *
  * This involves outputting a meaningful sequence of requests that are most likely to uncover bugs in the REST API, and
  * doing so in as few requests as possible (this is the most challenging part of AutoSpec, and always needs improving).
  */
class RequestGenerator(requestExecutor: EndpointRequestExecutor) {

  private type ParamResolution[A] = StateT[List, Map[EndpointParameterName, Json], A]

  def stream(session: Session): Stream[Task, Either[EndpointRequestFailure, EndpointRequestResponse]] =
    Stream.unfoldEval(initialState) { state =>
      nextRequest(session.schema, state).traverse {
        case Result(opportunities, request) =>
          requestExecutor
            .execute(session, request)
            .fold(
              e => e.asLeft  -> state,
              r => r.asRight -> state.append(opportunities, r)
            )
      }
    }

  private def nextRequest(schema: ApplicationSchema, state: State): Option[Result] = {
    val callableEndpoints = getCallableEndpoints(schema, state)
    val chosenEndpointOpt = weightedRandom(callableEndpoints)(x => endpointWeight(state, x.endpointId))
    chosenEndpointOpt.map { chosenEndpoint =>
      Result(
        Opportunities(
          possibleRequests = callableEndpoints.map(_.endpointId).toSet
        ),
        chosenEndpoint
          .possibleRequests
          .toList(
            // TODO: bias these based on how frequently available they are... but don't favour endpoints with more paramaters any more than those with equal! Currently we're just randomly selecting.
            Random.nextInt(chosenEndpoint.possibleRequests.size)
          )
      )
    }
  }

  private def getCallableEndpoints(schema: ApplicationSchema, state: State): List[CallableEndpoint] =
    schema.endpoints.flatMap(getCallableEndpoint(_, state))

  private def getCallableEndpoint(endpoint: EndpointDefinition, state: State): Option[CallableEndpoint] =
    getRequestCandidates(endpoint, state).toNEL.map(
      CallableEndpoint(endpoint.id, _)
    )

  private def getRequestCandidates(endpoint: EndpointDefinition, state: State): List[EndpointRequest] =
    generateParamCombinations(endpoint, state).run(Map.empty).flatMap {
      case (paramCombination, partiallyResolvedPredicates) =>
        val resolvedParamNames = paramCombination.keySet
        val unresolvedParams   = endpoint.parameters.filterNot(x => resolvedParamNames.contains(x.name))

        val paramsFromEndpoints = paramCombination.toList.map(_ :: Nil)
        val paramsFromRandom = unresolvedParams
          .map { parameter =>
            getParameterDomain(endpoint.id, parameter.`type`).map(parameterValue => parameter.name -> parameterValue)
          }

        val paramCombinations =
          cartesianProduct(
            paramsFromEndpoints ::: paramsFromRandom
          )

        val paramCombinationsOrNoParams =
          NonEmptyList
            .fromList(paramCombinations)
            .getOrElse(NonEmptyList.one(Nil))

        val paramCombinationsThatSatisfyPredicates =
          paramCombinationsOrNoParams.filter { params =>
            partiallyResolvedPredicates.forall(predicate => resolvePredicate(predicate, params.toMap))
          }

        paramCombinationsThatSatisfyPredicates.map { params =>
          EndpointRequest(
            endpoint.id,
            params.toMap
          )
        }
    }

  private def generateParamCombinations(
    endpoint: EndpointDefinition,
    state: State
  ): ParamResolution[List[I.Predicate]] =
    endpoint
      .preconditions
      .map(_.predicate)
      .traverse(resolveEndpointsInPredicate(_, state))

  private def resolvePredicate(predicate: I.Predicate, resolvedParams: Map[EndpointParameterName, Json]): Boolean =
    IntermediateSymbolResolver.resolvePredicate(predicate, resolvedParams)

  private def resolveEndpointsInPredicate(
    predicate: S.Predicate,
    state: State
  ): ParamResolution[I.Predicate] =
    SymbolConverter.convertPredicate(S, I)(predicate)(convertSchemaSymbols(_, state))

  private def convertSchemaSymbols(
    symbol: S.OwnSymbols,
    state: State
  ): ParamResolution[I.Symbol] =
    symbol match {
      case endpoint: S.Endpoint => resolveEndpointSymbol(endpoint, state).widen
      case S.Parameter(name)    => (I.Parameter(name): I.Symbol).pure[ParamResolution]
      case S.ResponseBody | S.StatusCode =>
        throw new Exception(s"Preconditions cannot contain a ${symbol.getClass.getSimpleName}")
    }

  private def resolveEndpointSymbol(
    endpoint: S.Endpoint,
    state: State
  ): ParamResolution[I.Literal] = {
    val previousRequests = state.responses.getOrElse(endpoint.endpointId, Queue.empty)
    val paramCombinations: StateT[List, Map[EndpointParameterName, Json], Map[
      EndpointParameterName,
      Either[EndpointParameterName, Json]
    ]] =
      endpoint.parameters.traverse {
        case x: S.Endpoint  => resolveEndpointSymbol(x, state).map(_.value.asRight[EndpointParameterName])
        case x: S.Parameter => x.name.asLeft[Json].pure[ParamResolution]
        case x: S.Literal   => x.value.asRight[EndpointParameterName].pure[ParamResolution]
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
    requests: Seq[EndpointRequestResponse],
    params: Map[EndpointParameterName, Either[EndpointParameterName, Json]]
  ): ParamResolution[I.Literal] = {
    val (variableParams, literalParams) = params.partitionEither(identity)
    val endpointParamsByVariable        = variableParams.swap.toList

    StateT { (state: Map[EndpointParameterName, Json]) =>
      matchRequestsByLiterals(requests, literalParams).map(state -> _).toList
    }.flatMap(
      matchRequestIfVariablesFit(_, endpointParamsByVariable).mapK(λ[Option ~> List](_.toList))
    )
  }

  private def matchRequestsByLiterals(
    requests: Seq[EndpointRequestResponse],
    partialParams: Map[EndpointParameterName, Json]
  ): Seq[EndpointRequestResponse] =
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

  // Returns a 'meaningfully sampled set' of all possible values this parameter can be. For boolean this is easy:
  // always return true and false. However, all other types have a much larger space, so we have to return a sample
  // that ideally represents meaningful boundaries.
  private def getParameterDomain(endpointId: EndpointId, parameterType: EndpointParameterType): List[Json] = {
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
          fields.view.mapValues(x => getParameterDomain(endpointId, x)).toMap
        )
          .map(Json.fromFields)
      case Array(elementTypes) =>
        val values =
          NonEmptyList
            .fromList(elementTypes)
            .getOrElse(NonEmptyList.of(Boolean, String, Int32))
            .toList
            .flatMap(getParameterDomain(endpointId, _))

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
    val count      = state.opportunities.take(maxBiasFactor).count(_.possibleRequests.contains(endpoint))
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

  private val initialState = State(Queue.empty, Queue.empty, 0)

  private case class CallableEndpoint(endpointId: EndpointId, possibleRequests: NonEmptyList[EndpointRequest])

  private case class Opportunities(possibleRequests: Set[EndpointId])

  private case class Result(
    opportunities: Opportunities,
    nextRequest: EndpointRequest
  )

  private case class State(history: Queue[EndpointRequestResponse], opportunities: Queue[Opportunities], size: Int) {
    lazy val responses: Map[EndpointId, Queue[EndpointRequestResponse]] = history.groupBy(_.request.endpointId)

    def append(o: Opportunities, r: EndpointRequestResponse): State =
      if (size === maxHistory)
        copy(
          history       = history.enqueue(r).dequeue._2,
          opportunities = opportunities.enqueue(o).dequeue._2
        )
      else
        copy(
          history       = history.enqueue(r),
          opportunities = opportunities.enqueue(o),
          size          = size + 1
        )

  }

}
