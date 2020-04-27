package autospec

import autospec.runtime.{ValidatedStreamFromTestPlan, _}
import autospec.runtime.resolvers.SymbolConverter
import autospec.schema.ApplicationSchema
import autospec.{RuntimeSymbolsExecuted => RE, RuntimeSymbolsIndexed => RI}
import cats.Id
import io.circe.Json
import monix.eval.Task

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
  def generateTestPlan(
    schema: ApplicationSchema,
    requests: List[EndpointRequestSymbolic]
  ): List[EndpointRequestWithChecks] = {
    import monix.execution.Scheduler.Implicits.global

    val requestExecutor = new EndpointRequestExecutor {
      override def execute(session: Session, request: EndpointRequest): Task[EndpointRequestResponse] =
        session.newRequestId().map { requestId =>
          EndpointRequestResponse(
            requestId,
            request,
            EndpointResponse(0, Json.Null, "null") // See comment above.
          )
        }
    }

    val validationStream = new ValidatedStreamFromTestPlan(requestExecutor)
    val pathExecutor     = new TestPlanExecutor(validationStream)
    val responses =
      for {
        session   <- Session.newSession(schema)
        responses <- pathExecutor.execute(session, requests, haltOnFailure = false)
      } yield responses

    responses.runSyncUnsafe().map { response =>
      EndpointRequestWithChecks(
        response.requestSymbolic,
        response.resolvedConditions.values.map {
          case (_, predicate) => convertToIndexedSymbol(predicate)
        }.toSet
      )
    }
  }

  private def convertToIndexedSymbol(
    symbol: RE.Predicate
  ): Id[RI.Predicate] =
    SymbolConverter
      .convertPredicate[Id, RE.type, RI.type](RE, RI)(symbol) {
        case RE.ResponseBody(requestId) => RI.ResponseBody(requestId.requestIndex)
        case RE.StatusCode(requestId)   => RI.StatusCode(requestId.requestIndex)
      }

}
