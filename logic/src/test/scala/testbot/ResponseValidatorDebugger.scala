package testbot

import cats.Id
import cats.implicits._
import io.circe.Json
import monix.eval.Task
import playground.{EndpointRequestResponse, ValidationStreamFromTestPlan}
import testbot.common.ULID
import testbot.runtime._
import testbot.runtime.resolvers.SymbolConverter
import testbot.schema.{ApplicationSchema, ConditionIdWithProvenance}
import testbot.{RuntimeSymbols => R, SchemaSymbols => S}

/**
 * Transforms the output of [[testbot.runtime.ResponseValidator]] into a more testable structure.
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
        response.resolvedConditions.keySet.map(resolve(schema, responses))
      )
    }
  }

  private def resolve(schema: ApplicationSchema, history: List[ValidatedRequestResponseWithSymbols])(conditionId: ConditionIdWithProvenance): R.Predicate = {
    val (response, requestIndex) = history.zipWithIndex.find(_._1.requestId === conditionId.provenance).get
    val endpoint                 = schema.endpoint(response.request.endpointId)
    val sPredicate               = endpoint.conditions(conditionId.conditionId)
    val rPredicate               = convertPredicate(history, requestIndex, sPredicate)
    rPredicate
  }

  private def convertPredicate(history: List[ValidatedRequestResponseWithSymbols], requestIndex: Int, predicate: S.Predicate): R.Predicate =
    SymbolConverter.convertPredicate(S, R)(predicate)(convertOwnSymbol(history, requestIndex, _))

  private def convertSymbol(history: List[ValidatedRequestResponseWithSymbols], requestIndex: Int, symbol: S.Symbol): R.Symbol =
    SymbolConverter.convertSymbol(S, R)(symbol)(convertOwnSymbol(history, requestIndex, _))

  private def convertOwnSymbol(history: List[ValidatedRequestResponseWithSymbols], requestIndex: Int, symbol: S.OwnSymbols): Id[R.Symbol] =
    symbol match {
      case S.ResponseBody    => R.ResponseBody(requestIndex).pure[Id]
      case S.StatusCode      => R.StatusCode(requestIndex).pure[Id]
      case S.Parameter(name) => R.Literal(history(requestIndex).request.parameterValues(name)).pure[Id]
      case S.Endpoint(endpointId, parameters, evaluateAfterExecution) =>
        // This part is far from perfect, but good enough to run our test DSL for 'ResponseValidatorSpec'.
        // Shortcoming: we resolve endpoints differently here to how they're actually resolved by the ResponseValidator
        // (the ResponseValidator resolves them much better!). Here we're matching requests by symbols, whereas in
        // ResponseValidator we're matching requests by resolved runtime values. For example, if we have the precondition
        // 'requires(foo(bar() == 42) == "buz")', and the symbolic requests 'foo(true)' and 'foo(false)', neither of
        // these will match here, whereas at runtime one will. However, our tests only use 'S.Literal' as request params
        // as of writing this, so we won't notice this shortcoming (and the approach below is simpler!).
        val resolvedParameters =
          parameters.view.mapValues(
            convertSymbol(
              history,
              requestIndex,
              _
            )
          ).toMap

        val historyWithIndex = history.zipWithIndex

        val scope =
          if (evaluateAfterExecution)
            historyWithIndex.drop(requestIndex)
          else
            historyWithIndex.take(requestIndex).reverse

        val resolvedRequest      = EndpointRequestSymbolic(endpointId, resolvedParameters)
        val resolvedRequestIndex = scope.find(_._1.requestSymbolic === resolvedRequest).get._2

        R.ResponseBody(resolvedRequestIndex).pure[Id]
    }
}
