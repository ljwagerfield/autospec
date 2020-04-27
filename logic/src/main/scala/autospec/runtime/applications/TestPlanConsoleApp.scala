package autospec.runtime.applications

import autospec.runtime.ConditionStatus.{Failed, Passed}
import autospec.runtime.exceptions.HttpClientExceptionWithSymbols
import autospec.runtime.{ValidatedStreamFromRequestStream, _}
import autospec.schema.ApplicationSchema
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.client.asynchttpclient.AsyncHttpClient

class TestPlanConsoleApp()(implicit scheduler: Scheduler) {
  private val printer: SymbolPrinter = ScalaSymbolPrinter

  def run(schema: ApplicationSchema, paths: List[TestPlan]): Task[Unit] =
    AsyncHttpClient.resource[Task]().use { httpClient =>
      val httpRequestExecutor     = new HttpRequestExecutor(httpClient)
      val endpointRequestExecutor = new EndpointRequestExecutorImpl(httpRequestExecutor)
      val validationStream        = new ValidatedStreamFromRequestStream(endpointRequestExecutor)
      val testPathExecutor        = new TestPlanExecutor(validationStream)
      for {
        session     <- Session.newSession(schema)
        testResults <- testPathExecutor.executeMany(session, paths, haltOnFailure = true)
      } yield printResults(schema, paths, testResults)
    }

  private def printResults(
    schema: ApplicationSchema,
    paths: List[TestPlan],
    testResults: Map[TestPlanId, (Option[HttpClientExceptionWithSymbols], List[ValidatedRequestResponseWithSymbols])]
  ): Unit = {
    def color(failed: Boolean): String = if (failed) Console.RED else Console.GREEN

    def printRequest(failed: Boolean, index: Long, request: EndpointRequestSymbolic): Unit =
      println(s"${color(failed)}    $index: ${printer.print(request, index)}")

    println(s"${color(false)}Tests:")
    println()

    paths.foreach { path =>
      println(s"${color(false)}  ${path.id.value}:")

      val (errorOpt, pathResult) = testResults(path.id)
      val allConditions          = pathResult.flatMap(_.resolvedConditions).toMap

      pathResult.foreach { result =>
        val index            = result.requestId.requestIndex.index
        val request          = result.requestSymbolic
        val isTestPathFailed = result.isFailed
        val conditions       = schema.endpoint(request.endpointId).conditions

        printRequest(isTestPathFailed, index, request)

        conditions.foreach {
          case (conditionId, predicate) =>
            val (icon, color) = allConditions.get(conditionId.withProvenance(result.requestId)).map(_._1) match {
              case None         => "?" -> Console.YELLOW
              case Some(Failed) => "✖" -> Console.RED
              case Some(Passed) => "✔" -> Console.GREEN
            }
            println(s"$color       $icon ${printer.print(predicate)}")
        }
      }

      print(Console.RESET)

      if (pathResult.exists(_.isFailed)) {
        println(s"${Console.RESET}    responses:")
        pathResult.zipWithIndex.foreach {
          case (response, index) =>
            println(s"      $index: $response")
        }
      }

      errorOpt.foreach { error =>
        printRequest(true, pathResult.size.toLong, error.request)
        println(
          s"${color(true)}       ▣ Aborted: request failed after several retry attempts."
        )
      }

      println()
    }

    val failureCount =
      testResults.values.count {
        case (Some(_), _)    => true
        case (None, results) => results.exists(_.isFailed)
      }

    if (failureCount === 0)
      print(s"${color(false)}All tests passed.")
    else
      print(s"${color(true)}Uh oh! You have $failureCount failed test${if (failureCount === 1) "" else "s"}.")

    println(Console.RESET)
  }

}
