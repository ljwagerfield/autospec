package autospec

import autospec.ResponseValidatorSpecBase.TestInstruction
import autospec.ResponseValidatorSpecBase.TestInstruction.{RunAssertion, SimulateRequestFailure}
import autospec.runtime.exceptions.{EndpointRequestFailure, HttpRequestFailure}
import autospec.runtime.{ValidatedStreamFromRequestStream, _}
import autospec.runtime.resolvers.SymbolConverter
import autospec.schema.ApplicationSchema
import autospec.{RuntimeSymbolsExecuted => RE, RuntimeSymbolsIndexed => RI}
import cats.Id
import cats.data.EitherT
import cats.implicits._
import io.circe.Json
import monix.eval.Task
import org.http4s.{Method, Uri}

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
  def executePlanAndReturnCheckedConditions(
    schema: ApplicationSchema,
    requests: Seq[(EndpointRequestSymbolic, TestInstruction)]
  ): List[Option[EndpointRequestWithChecks]] = {
    import monix.execution.Scheduler.Implicits.global

    val requestExecutor = new EndpointRequestExecutor {
      override def execute(
        session: Session,
        request: EndpointRequest
      ): EitherT[Task, EndpointRequestFailure, EndpointRequestResponse] =
        EitherT(
          session.newRequestId().map { requestId =>
            val (_, testInstruction) = requests(requestId.requestIndex.index.toInt)
            testInstruction match {
              case SimulateRequestFailure =>
                EndpointRequestFailure(
                  request,
                  requestId,
                  HttpRequestFailure(
                    "Simulated request failure",
                    RequestSummary(Method.GET, Uri.unsafeFromString("http://dummyrequest")),
                    new Exception()
                  )
                ).asLeft

              case _: RunAssertion =>
                EndpointRequestResponse(
                  requestId,
                  request,
                  EndpointResponse(0, Json.Null, "null") // See comment above.
                ).asRight
            }
          }
        )
    }

    val testPath         = requests.map(_._1).toList
    val validationStream = new ValidatedStreamFromRequestStream(requestExecutor)
    val pathExecutor     = new TestPlanExecutor(validationStream)
    val responses =
      for {
        session   <- Session.newSession(schema)
        responses <- pathExecutor.execute(session, testPath, haltOnFailure = false)
      } yield responses

    val responseList = responses.runSyncUnsafe()

    responseList.map(_.toOption.map { response =>
      EndpointRequestWithChecks(
        response.requestSymbolic,
        response.resolvedConditions.values.map {
          case (_, predicate) => convertToIndexedSymbol(predicate)
        }.toSet
      )
    })
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
