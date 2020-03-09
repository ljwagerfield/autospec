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
import spike.{ResolvedSymbols => R, SchemaSymbols => S}

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

  }

  private def resolvePredicate(predicate: R.Predicate, resolvedParameters: Map[EndpointParameterName, Json]): Boolean = {

  }

  private def resolveEndpointsInPredicate(
    predicate: S.Predicate,
    potentialDependencies: List[EndpointRequestResponse],
    resolvedParameters: Map[EndpointParameterName, Json]
  ): Option[(R.Predicate, Map[EndpointParameterName, Json])] = {
    // Constrain endpoint params to either literals, params, or other endpoints, but cannot be anything else.
    // Otherwise we'd need to deal with situations like this: 'endpoint(concat(param1, param2))'


    // IF resolvedParameters already contains an entry, then see if that value matches the value in the potentialDependencies'
    // parameter list, and if so, use it. Only error/return none if a resolvedParameters exists that doesn't match the param
    // in the potentialDependencies.

    // 1. Substitute 'Endpoint(...)' with 'Literal(
  }

  private def resolveEndpointsInSymbol(
    symbol: S.Symbol,
    potentialDependencies: List[EndpointRequestResponse],
    resolvedParameters: Map[EndpointParameterName, Json]
  ): Option[(R.Symbol, Map[EndpointParameterName, Json])] = {
    // 1. Substitute 'Endpoint(...)' with 'Literal(
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
}
