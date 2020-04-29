package autospec.runtime

import autospec.IntermediateSymbols.{Predicate => IP}
import autospec.SchemaSymbols.{Predicate => SP}
import autospec.common.FunctorExtensions._
import autospec.common.MathUtils._
import autospec.runtime.RequestGenerator._
import autospec.runtime.exceptions.EndpointRequestFailure
import autospec.runtime.resolvers.IntermediateSymbolResolver
import autospec.schema._
import autospec.{IntermediateSymbols => I, SchemaSymbols => S}
import cats.data.NonEmptyList
import cats.implicits._
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

  private def getRequestCandidates(endpoint: EndpointDefinition, state: State): List[EndpointRequest] = {
    val paramsIfEndpointDependenciesMet =
      endpoint.preconditions.foldLeftM((List.empty[I.Predicate], Map.empty[EndpointParameterName, Json])) {
        (accum, precondition) =>
          val (resolvedPredicates, resolvedParams) = accum
          resolveEndpointsInPredicate(precondition.predicate, state, resolvedParams).map {
            case (resolvedPredicate, updatedResolvedParams) =>
              (
                resolvedPredicate :: resolvedPredicates,
                updatedResolvedParams
              )
          }
      }

    paramsIfEndpointDependenciesMet.flatMap {
      case (partiallyResolvedPredicates, resolvedParams) =>
        val resolvedParamNames = resolvedParams.keySet
        val unresolvedParams   = endpoint.parameters.filterNot(x => resolvedParamNames.contains(x.name))

        val paramsFromEndpoints = resolvedParams.toList.map(_ :: Nil)
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
  }

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

  private def resolvePredicate(predicate: I.Predicate, resolvedParams: Map[EndpointParameterName, Json]): Boolean =
    IntermediateSymbolResolver.resolvePredicate(predicate, resolvedParams)

  private def resolveEndpointsInPredicate(
    predicate: S.Predicate,
    state: State,
    resolvedParams: Map[EndpointParameterName, Json]
  ): List[(I.Predicate, Map[EndpointParameterName, Json])] = {
    val symbolConverter = new SymbolConverter(state, resolvedParams)
    import symbolConverter._
    predicate match {
      case SP.Equals(left, right)        => convert2(IP.Equals, left, right)
      case SP.And(left, right)           => convert2(IP.And, left, right)
      case SP.Or(left, right)            => convert2(IP.Or, left, right)
      case SP.Not(pred)                  => convert(IP.Not, pred)
      case SP.Exists(symbol, pred)       => convert2(IP.Exists, symbol, pred)
      case SP.Contains(collection, item) => convert2(IP.Contains, collection, item)
    }
  }

  private def resolveEndpointsInSymbol(
    symbol: S.Symbol,
    state: State,
    resolvedParams: Map[EndpointParameterName, Json]
  ): List[(I.Symbol, Map[EndpointParameterName, Json])] = {
    val symbolConverter = new SymbolConverter(state, resolvedParams)
    import symbolConverter._
    symbol match {
      case S.ResponseBody | S.StatusCode =>
        throw new Exception(s"Preconditions cannot contain ${symbol.getClass.getSimpleName} symbols.")

      case endpoint: S.Endpoint => resolveEndpointSymbol(endpoint, state, resolvedParams)

      case S.Parameter(name)           => List(I.Parameter(name)           -> resolvedParams)
      case S.Literal(value)            => List(I.Literal(value)            -> resolvedParams)
      case S.LambdaParameter(distance) => List(I.LambdaParameter(distance) -> resolvedParams)
      case S.Map(symbol, path)         => convert2(I.Map, symbol, path)
      case S.FlatMap(symbol, path)     => convert2(I.FlatMap, symbol, path)
      case S.Flatten(symbol)           => convert(I.Flatten, symbol)
      case S.Find(symbol, predicate)   => convert2(I.Find, symbol, predicate)
      case S.Count(symbol)             => convert(I.Count, symbol)
      case S.Distinct(symbol)          => convert(I.Distinct, symbol)
      case S.Add(left, right)          => convert2(I.Add, left, right)
      case S.Subtract(left, right)     => convert2(I.Subtract, left, right)
      case S.Multiply(left, right)     => convert2(I.Multiply, left, right)
      case S.Divide(left, right)       => convert2(I.Divide, left, right)
      case S.Concat(left, right)       => convert2(I.Concat, left, right)
      case predicate: S.Predicate      => resolveEndpointsInPredicate(predicate, state, resolvedParams)
    }
  }

  private def resolveEndpointSymbol(
    endpoint: S.Endpoint,
    state: State,
    resolvedParams: Map[EndpointParameterName, Json]
  ): List[(I.Literal, Map[EndpointParameterName, Json])] = {
    val previousRequests = state.responses.getOrElse(endpoint.endpointId, Queue.empty)
    val paramCombinations =
      endpoint.parameters.toList.foldLeftM(
        Map.empty[EndpointParameterName, Either[EndpointParameterName, Json]] -> resolvedParams
      ) { (accum, parameter) =>
        val (endpointParams, resolvedParams) = accum
        val (paramName, paramValue)          = parameter
        paramValue match {
          case x: S.Endpoint =>
            resolveEndpointSymbol(x, state, resolvedParams).map {
              case (endpointResponse, resolvedParams) =>
                (
                  endpointParams ++ Map(paramName -> endpointResponse.value.asRight),
                  resolvedParams
                )
            }
          case x: S.Parameter =>
            List(
              (
                endpointParams ++ Map(paramName -> x.name.asLeft),
                resolvedParams
              )
            )
          case x: S.Literal =>
            List(
              (
                endpointParams ++ Map(paramName -> x.value.asRight),
                resolvedParams
              )
            )
          // TODO: we must support 'Map' next, so we can do things like 'requires(getPreset(getNeg(x).presetId))' -- see 'presetId' -- this is a simple Map operation.
          case x =>
            throw new UnsupportedOperationException(
              s"Endpoint references inside preconditions must currently use either literals or other endpoint references as parameters. The use of ${x.getClass.getSimpleName} is currently unsupported."
            )
        }
      }

    paramCombinations.flatMap {
      case (endpointParams, resolvedParams) =>
        matchRequestsByParams(previousRequests, endpointParams).map {
          case (endpointResponse, resolvedChildParams) =>
            (
              endpointResponse,
              resolvedParams ++ resolvedChildParams
            )
        }
    }
  }

  private def matchRequestsByParams(
    requests: Seq[EndpointRequestResponse],
    params: Map[EndpointParameterName, Either[EndpointParameterName, Json]]
  ): Seq[(I.Literal, Map[EndpointParameterName, Json])] = {
    val (variableParams, literalParams) = params.partitionEither(identity)
    val endpointParamsByVariable        = variableParams.swap.toList
    matchRequestsByLiterals(requests, literalParams).flatMap(
      matchRequestIfVariablesFit(_, endpointParamsByVariable)
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
  ): Option[(I.Literal, Map[EndpointParameterName, Json])] = {
    val resolvedVariableParamsOpt =
      endpointParamsByVariable.foldLeftM[Option, Map[EndpointParameterName, Json]](Map.empty) { (accum, params) =>
        val (variableParam, endpointParams) = params
        val paramValues                     = endpointParams.map(request.request.parameterValues.apply).toList
        paramValues match {
          case paramValue :: Nil =>
            Some(
              accum ++ Map(variableParam -> paramValue)
            )
          case _ =>
            // Elide this request, as the same variable is used for multiple parameters, but in this request,
            // those parameters have different values, meaning they cannot be mapped to the same / a single variable.
            // E.g. the variable 'x' is used in 'foo(x, x)' but this request is 'foo(1,2)', so 'x' does not fit.
            None
        }
      }

    resolvedVariableParamsOpt.map { resolvedWildcardParams =>
      (
        I.Literal(request.response.body),
        resolvedWildcardParams
      )
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

  class SymbolConverter(state: State, resolvedParams: Map[EndpointParameterName, Json]) {
    type Convert[A, B] = (A, Map[EndpointParameterName, Json]) => List[(B, Map[EndpointParameterName, Json])]

    implicit val convertSymbol: Convert[S.Symbol, I.Symbol]          = resolveEndpointsInSymbol(_, state, _)
    implicit val convertPredicate: Convert[S.Predicate, I.Predicate] = resolveEndpointsInPredicate(_, state, _)

    def convert[A, A2, R](newType: A2 => R, a: A)(implicit
      atoa: Convert[A, A2]
    ): List[(R, Map[EndpointParameterName, Json])] =
      atoa(a, resolvedParams).map {
        case (a2, resolvedParams2) =>
          newType(a2) -> resolvedParams2
      }

    def convert2[A, B, A2, B2, R](newType: (A2, B2) => R, a: A, b: B)(implicit
      atoa: Convert[A, A2],
      btob: Convert[B, B2]
    ): List[(R, Map[EndpointParameterName, Json])] =
      for {
        (a2, resolvedParams2) <- atoa(a, resolvedParams)
        (b2, resolvedParams3) <- btob(b, resolvedParams2)
      } yield newType(a2, b2) -> resolvedParams3

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
