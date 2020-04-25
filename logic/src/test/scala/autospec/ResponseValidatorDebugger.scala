package autospec

import autospec.common.ULID
import autospec.runtime._
import autospec.runtime.resolvers.SymbolConverter
import autospec.schema.ApplicationSchema
import autospec.{RuntimeSymbolsExecuted => RE, RuntimeSymbolsIndexed => RI}
import cats.Id
import cats.implicits._
import io.circe.Json
import monix.eval.Task
import playground.{EndpointRequestResponse, ValidationStreamFromTestPlan}
/**
 * Transforms the output of [[autospec.runtime.ResponseValidator]] into a more testable structure.
 */
object ResponseValidatorDebugger {
  /**
   * Generates the test plan followed by the response validator, so we can verify the logic the response validator
   * follows for forward lookups (postconditions), reverse lookups (preconditions and postconditions), and performing
   * these lookups whilst dealing with mutable and pure endpoints.
   *
   * This means we only need to return the details of which predicates are checked at each request: we're not interested
   * in evaluating the predicates at this point, so we can mock all endpoint responses to 'null', as we don't mind if
   * they all fail.
   *
   * See the result: notice how we're just returning the checks, and not the results of evaluating those checks.
   */
  def generateTestPlan(schema: ApplicationSchema, requests: List[EndpointRequestSymbolic]): List[EndpointRequestWithChecks] = {
    // Ensures request IDs are causally ordered (as we're generating them in tight loops, and many may occur per ms).
    import IncrementalClock.instance
    import monix.execution.Scheduler.Implicits.global

    val requestExecutor = new EndpointRequestExecutor {
      override def execute(schema: ApplicationSchema, request: EndpointRequest): Task[EndpointRequestResponse] =
        ULID.next[Task].map(EndpointRequestId.apply).map { requestId =>
          EndpointRequestResponse(
            requestId,
            request,
            EndpointResponse(0, Json.Null, "null") // See comment above.
          )
        }
    }

    val validationStream = new ValidationStreamFromTestPlan(requestExecutor)
    val pathExecutor     = new TestPlanExecutor(validationStream)
    val responses = pathExecutor.execute(schema, requests, haltOnFailure = false).runSyncUnsafe()
    responses.map { response =>
      EndpointRequestWithChecks(
        response.requestSymbolic,
        response.resolvedConditions.values.map { case (_, predicate) =>
          convertToIndexedSymbol(
            id => responses.indexWhere(_.requestId === id).toLong,
            predicate
          )
        }.toSet
      )
    }
  }

  private def convertToIndexedSymbol(
    getResponse: EndpointRequestId => EndpointRequestIndex,
    symbol: RE.Predicate
  ): Id[RI.Predicate] =
    SymbolConverter
      .convertPredicate[Id, RE.type, RI.type](RE, RI)(symbol) {
          case RE.ResponseBody(requestId) => RI.ResponseBody(getResponse(requestId))
          case RE.StatusCode(requestId)   => RI.StatusCode(getResponse(requestId))
        }
}
