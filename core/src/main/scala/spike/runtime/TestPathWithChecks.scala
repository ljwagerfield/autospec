package spike.runtime

import io.circe.Json
import spike.schema.{ApplicationSchema, ConditionId, Precondition}
import spike.RuntimeSymbols._
import spike.RuntimeSymbols.Predicate._
import scala.collection.immutable.{Map => ScalaMap}
import cats.implicits._

case class TestPathWithChecks(id: TestPathId, requests: List[EndpointRequestWithChecks])

object TestPathWithChecks {
  private type RequestIndex            = Int
  private type PostconditionsByRequest = ScalaMap[RequestIndex, ScalaMap[ConditionId, Predicate]]

  def apply(schema: ApplicationSchema, testPath: TestPath): TestPathWithChecks =
    TestPathWithChecks(
      testPath.id,
      testPath
        .requests
        .foldLeft(State(0, Nil, ScalaMap.empty))(addChecksToRequest(schema, testPath, _, _))
        .requests
    )

  private def addChecksToRequest(schema: ApplicationSchema, testPath: TestPath, state: State, request: EndpointRequest): State = {
    val requestIndex = state.currentRequestIndex
    val endpoint     = schema.endpoint(request.endpointId)
    val (preconditionScope, currentRequest :: postconditionScope) = testPath.requests.splitAt(requestIndex)

    val preconditions =
      endpoint.preconditionMap.flatMap[ConditionId, Predicate] { case (conditionId, Precondition(predicate, expectedStatus)) =>
        for {
          success <- SymbolConverter.convertToRuntimePredicate(
            currentRequest,
            preconditionScope,
            Nil,
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

    val newPostconditions =
      endpoint.postconditionMap.flatMap[ConditionId, Predicate] { case (conditionId, predicate) =>
        for {
          success <- SymbolConverter.convertToRuntimePredicate(
            currentRequest,
            preconditionScope,
            postconditionScope,
            predicate
          )
        } yield (
          conditionId,
          success
        )
      }
      .groupBy { case (_, predicate) =>
        // Associate postconditions with the last request they reference internally (if any), so the condition can be
        // be checked after that endpoint is executed (it cannot be checked before). If no endpoints are referenced,
        // then check the postcondition immediately after the current request (i.e. 'getOrElse(requestIndex)').
        maxRequestIndex(predicate).getOrElse(requestIndex)
      }

    val mergedPostconditions   = mergePostconditions(state.deferredPostconditions, newPostconditions)
    val ownPostconditions      = mergedPostconditions.getOrElse(requestIndex, ScalaMap.empty)
    val deferredPostconditions = mergedPostconditions - requestIndex

    val requestWithChecks = EndpointRequestWithChecks(
      request,
      preconditions ++ ownPostconditions
    )

    State(
      currentRequestIndex    = requestIndex + 1,
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
      case Map(symbol, _)             => maxRequestIndex(symbol)
      case FlatMap(symbol, _)         => maxRequestIndex(symbol)
      case Flatten(symbol)            => maxRequestIndex(symbol)
      case Find(symbol, predicate)    => max(maxRequestIndex(symbol), maxRequestIndex(predicate))
      case Count(symbol)              => maxRequestIndex(symbol)
      case Distinct(symbol)           => maxRequestIndex(symbol)
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

  case class State(
    currentRequestIndex: Int,
    requests: List[EndpointRequestWithChecks],
    deferredPostconditions: PostconditionsByRequest
  )
}