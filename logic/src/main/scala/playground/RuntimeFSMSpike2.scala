package playground

import cats.data.NonEmptyList
import cats.implicits._
import io.circe.Json
import monix.eval.Task
import monix.execution.Scheduler
import playground.SymbolExtensions._
import spike.SchemaSymbols.Endpoint
import spike.common.FunctorExtensions._
import spike.common.MathUtils
import spike.runtime.EndpointRequestExecutor
import spike.schema._
import spike.{ResolvedPreconditionSymbols => R, SchemaSymbols => S}
import spike.ResolvedPreconditionSymbols.{Predicate => RP}
import spike.SchemaSymbols.{Predicate => SP}

import scala.util.Random

class RuntimeFSMSpike2(requestExecutor: EndpointRequestExecutor)(implicit scheduler: Scheduler) {
  /**
   * The maximum factor to use for biasing the selection of an infrequently-seen endpoint vs a frequently-seen endpoint.
   * For example, a factor of 10 says "if we've seen endpoint A once, and endpoint B every time, then give endpoint
   * A 10x the chance of being called than endpoint B".
   */
  private val maxBiasFactor = 10

  /**
   * Returns if/when there are no more endpoints to call.
   *
   * For most APIs, this method will never return.
   */
  def run(schema: ApplicationSchema): Task[Unit] =
    Task.tailRecM((ApplicationState.empty, List.empty[RequestRound])) { case (state, rounds) =>
      {
        for {
          requestRound <- nextRequest(schema, state, rounds).toRight(()).toEitherT[Task]
          response     <- requestExecutor.execute(schema, requestRound.chosenRequest).toEitherT[Unit]
          // Todo: perform condition validation!
          // Todo: truncate history and state beyond 10,000 requests (or store to disk)
        } yield (
          state.add(response),
          requestRound :: rounds
        )
      }.swap.value
    }

  def nextRequest(schema: ApplicationSchema, state: ApplicationState, history: List[RequestRound]): Option[RequestRound] = {
    val callableEndpoints = getCallableEndpoints(schema, state)
    val chosenEndpointOpt = MathUtils.weightedRandom[CallableEndpoint](callableEndpoints, x => endpointWeight(schema, state, history, x.endpointId))
    chosenEndpointOpt.map { chosenEndpoint =>
      RequestRound(
        callableEndpoints = callableEndpoints.map(_.endpointId).toSet,
        chosenRequest     = chosenEndpoint
          .possibleRequests
          .toList(
            // We don't do any biasing on the different request options for a single endpoint. We don't _think_ there's
            // any benefit of biasing at this level (but we may be wrong!).
            Random.nextInt(chosenEndpoint.possibleRequests.size)
          )
      )
    }
  }

  private def getCallableEndpoints(schema: ApplicationSchema, state: ApplicationState): List[CallableEndpoint] =
    schema.endpoints.flatMap(getCallableEndpoint(_, state))

  private def getCallableEndpoint(endpoint: EndpointDefinition, state: ApplicationState): Option[CallableEndpoint] = {
    val requestDependencyCombinations =
      getPotentialRequestDependencies(
        getEndpointDependencies(endpoint),
        state
      )

    val requestCandidates =
      requestDependencyCombinations.flatMap(
        getRequestCandidate(endpoint, _)
      )

    requestCandidates.toNEL.map(
      CallableEndpoint(endpoint.id, _)
    )
  }

  private def getEndpointDependencies(endpoint: EndpointDefinition): List[EndpointId] =
    endpoint.preconditions.flatMap(getEndpointDependencies)

  private def getEndpointDependencies(precondition: Precondition): List[EndpointId] =
    precondition.predicate.toList.collect {
      case x: Endpoint => x.endpointId
    }

  private def getPotentialRequestDependencies(endpoints: List[EndpointId], state: ApplicationState): List[List[EndpointRequestResponse]] =
    MathUtils.cartesianProduct(
      endpoints.map { endpoint =>
        state.requestsByEndpoint.getOrElse(endpoint, Nil)
      }
    )

  private def getRequestCandidate(endpoint: EndpointDefinition, potentialDependencies: List[EndpointRequestResponse]): Option[EndpointRequest] = {
    val paramsIfEndpointDependenciesMet =
      endpoint.preconditions.foldLeftM((List.empty[R.Predicate], Map.empty[EndpointParameterName, Json])) { (accum, precondition) =>
        val (resolvedPredicates, resolvedParams) = accum
        resolveEndpointsInPredicate(precondition.predicate, potentialDependencies, resolvedParams).map {
          case (resolvedPredicate, updatedResolvedParams) => (
            resolvedPredicate :: resolvedPredicates,
            updatedResolvedParams
          )
        }
      }

    paramsIfEndpointDependenciesMet.flatMap { case (partiallyResolvedPredicates, resolvedParams) =>
      val resolvedParamNames  = resolvedParams.keySet
      val unresolvedParams    = endpoint.parameters.filterNot(x => resolvedParamNames.contains(x.name))

      val paramsFromEndpoints = resolvedParams.toList.map(_ :: Nil)
      val paramsFromRandom    = unresolvedParams
        .map { parameter =>
          getRandomValues(parameter).map { parameterValue =>
            parameter.name -> parameterValue
          }
        }

      val paramCombinations =
        MathUtils.cartesianProduct(
          paramsFromEndpoints ::: paramsFromRandom
        )

      val paramCombinationsOrNoParams =
        NonEmptyList
          .fromList(paramCombinations)
          .getOrElse(NonEmptyList.one(Nil))

      // Todo: We're always exercising the same branch here as we're selecting the first random parameters that work.
      // We should instead select these randomly, but obviously need to consider dimensionality, etc.
      val chosenParams =
        paramCombinationsOrNoParams.find { params =>
          partiallyResolvedPredicates.forall { predicate =>
            resolvePredicate(predicate, params.toMap)
          }
        }

      chosenParams.map { params =>
        EndpointRequest(
          endpoint.id,
          params.toMap
        )
      }
    }
  }

  private def getRandomValues(parameterType: EndpointParameter): List[Json] = {
    // Todo: add type information to 'EndpointParameter' such that we can generate sensible values.
  }

  private def resolvePredicate(predicate: R.Predicate, resolvedParams: Map[EndpointParameterName, Json]): Boolean = {

  }

  private def resolveEndpointsInPredicate(
    predicate: S.Predicate,
    potentialDependencies: List[EndpointRequestResponse],
    resolvedParams: Map[EndpointParameterName, Json]
  ): Option[(R.Predicate, Map[EndpointParameterName, Json])] = {
    val symbolConverter = new SymbolConverter(potentialDependencies, resolvedParams)
    import symbolConverter._
    predicate match {
      case SP.Equals(left, right)        => convert2(RP.Equals, left, right)
      case SP.And(left, right)           => convert2(RP.And, left, right)
      case SP.Or(left, right)            => convert2(RP.Or, left, right)
      case SP.Not(pred)                  => convert(RP.Not, pred)
      case SP.Exists(symbol, pred)       => convert2(RP.Exists, symbol, pred)
      case SP.Contains(collection, item) => convert2(RP.Contains, collection, item)
    }
  }

  private def resolveEndpointsInSymbol(
    symbol: S.Symbol,
    potentialDependencies: List[EndpointRequestResponse],
    resolvedParams: Map[EndpointParameterName, Json]
  ): Option[(R.Symbol, Map[EndpointParameterName, Json])] = {
    val symbolConverter = new SymbolConverter(potentialDependencies, resolvedParams)
    import symbolConverter._
    symbol match {
      case S.ResponseBody | S.StatusCode => throw new Exception(s"Preconditions cannot contain $symbol symbols.")
      case endpoint: S.Endpoint          => resolveEndpointSymbol(endpoint, potentialDependencies, resolvedParams)

      case S.Parameter(name) =>
        Some {
          val paramOrResolved =
            resolvedParams
              .get(name)
              .map(json => R.Literal(json))
              .getOrElse(R.Parameter(name))

          paramOrResolved -> resolvedParams
        }

      case S.Literal(value)            => Some(R.Literal(value) -> resolvedParams)
      case S.LambdaParameter(distance) => Some(R.LambdaParameter(distance) -> resolvedParams)
      case S.Map(symbol, path)         => convert2(R.Map, symbol, path)
      case S.FlatMap(symbol, path)     => convert2(R.FlatMap, symbol, path)
      case S.Flatten(symbol)           => convert(R.Flatten, symbol)
      case S.Find(symbol, predicate)   => convert2(R.Find, symbol, predicate)
      case S.Count(symbol)             => convert(R.Count, symbol)
      case S.Distinct(symbol)          => convert(R.Distinct, symbol)
      case S.Prepend(item, collection) => convert2(R.Prepend, item, collection)
      case S.Append(collection, item)  => convert2(R.Append, collection, item)
      case S.Concat(left, right)       => convert2(R.Concat, left, right)
      case predicate: S.Predicate      => resolveEndpointsInPredicate(predicate, potentialDependencies, resolvedParams)
    }
  }

  private def resolveEndpointSymbol(
    endpoint: S.Endpoint,
    potentialDependencies: List[EndpointRequestResponse],
    resolvedParams: Map[EndpointParameterName, Json]
  ): Option[(R.Literal, Map[EndpointParameterName, Json])] = {
    // 1.

    // TODO:is there a problem if there's only 1 dependency for each endpoint. E.g.
    // requires(getNeg(paramA).groupId === getNeg(paramB).groupId)
    // In this scenario, we have 2 unique variables that we want permutations of... we don't want paramA and paramB to always be equal (they will currently).

    endpoint.parameters

    // 1. Substitute 'Endpoint(...)' with 'Literal(

    // IF resolvedParameters already contains an entry, then see if that value matches the value in the potentialDependencies'
    // parameter list, and if so, use it. Only error/return none if a resolvedParameters exists that doesn't match the param
    // in the potentialDependencies.

    // throw exception if endpoint params are none of: literal, param, or other endpoint.
    // Otherwise we'd need to deal with situations like this: 'endpoint(concat(param1, param2))'
  }

  private def endpointWeight(
    schema: ApplicationSchema,
    state: ApplicationState,
    history: List[RequestRound],
    endpoint: EndpointId
  ): Int = {
    val count      = history.take(maxBiasFactor).count(_.callableEndpoints.contains(endpoint)) // TODO: use 'contains_' instead.
    val penalty    = count + 1
    val maxPenalty = maxBiasFactor + 1

    // Random (no biasing of less-frequently available endpoints)
    //         Empirical performance (lower is better): 20,000 requests to find all unique 3-strands in a sample API.
    1

    // Relative (endpoint A that has appeared Nx less than another endpoint B will have Nx more chance
    //           of being called in this iteration than endpoint B).
    //           Empirical performance (lower is better): 10,000 requests to find all unique 3-strands in a sample API.
    Math.round((1D / (penalty.toDouble / maxPenalty.toDouble)) * 100).toInt

    // Linear (least-frequent endpoint has Nx more chance of being called than most frequent endpoint,
    //         but only N/(N-1)x chance than second-least-frequent endpoint).
    //         Empirical performance (lower is better): 5,000 requests to find all unique 3-strands in a sample API.
    maxPenalty - count
  }

  class SymbolConverter(potentialDependencies: List[EndpointRequestResponse], resolvedParams: Map[EndpointParameterName, Json]) {
    type Convert[A, B] = (A, Map[EndpointParameterName, Json]) => Option[(B, Map[EndpointParameterName, Json])]

    implicit val convertSymbol: Convert[S.Symbol, R.Symbol] =
      resolveEndpointsInSymbol(_, potentialDependencies, _)

    implicit val convertPredicate: Convert[S.Predicate, R.Predicate] =
      resolveEndpointsInPredicate(_, potentialDependencies, _)

    def convert[A, A2, R](newType: (A2) => R, a: A)
                         (implicit atoa: Convert[A, A2]): Option[(R, Map[EndpointParameterName, Json])] =
      atoa(a, resolvedParams).map { case (a2, resolvedParams2) =>
        newType(a2) -> resolvedParams2
      }

    def convert2[A, B, A2, B2, R](newType: (A2, B2) => R, a: A, b: B)
                                 (implicit atoa: Convert[A, A2], btob: Convert[B, B2]): Option[(R, Map[EndpointParameterName, Json])] =
      for {
        (a2, resolvedParams2) <- atoa(a, resolvedParams)
        (b2, resolvedParams3) <- btob(b, resolvedParams2)
      } yield {
        newType(a2, b2) -> resolvedParams3
      }
  }
}
