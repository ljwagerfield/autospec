package spike.runtime

import cats.data.Chain
import io.circe.Json
import spike.schema.{ApplicationSchema, ConditionId, Precondition}
import spike.RuntimeSymbols._
import spike.RuntimeSymbols.Predicate._
import spike.common.FunctorExtensions._

import scala.collection.immutable.{Map => ScalaMap}
import cats.implicits._
import spike.runtime.SymbolConverter.SymbolConverterError.{UnresolvedForwardLookup, UnresolvedReverseLookup}

object TestPlanGenerator {
  private type RequestIndex            = Int
  private type PostconditionsByRequest = ScalaMap[RequestIndex, ScalaMap[ConditionId, Predicate]]

  def generate(schema: ApplicationSchema, paths: List[TestPath]): TestPlan =
    TestPlan(
      schema,
      paths.map(addChecksToTestPath(schema, _))
    )

  private def addChecksToTestPath(schema: ApplicationSchema, testPath: TestPath): TestPathWithChecks =
    TestPathWithChecks(
      testPath.id,
      testPath
        .requests
        .foldLeft(State(0, 0, Chain.empty, testPath.requests, Chain.empty, ScalaMap.empty))(addChecksToRequest(schema, _, _))
        .requests
    )

  private def addChecksToRequest(schema: ApplicationSchema, state: State, request: EndpointRequestOld): State = {
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
          // Clear all previously deferred postconditions (mutations invalidated them).
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

    State(
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

  private case class State(
    currentRequestIndex: Int,
    preconditionOffset: Int,
    preconditionScope: Chain[EndpointRequestOld],
    postconditionScope: Chain[EndpointRequestOld],
    requests: Chain[EndpointRequestWithChecks],
    deferredPostconditions: PostconditionsByRequest
  )
}