package autospec.runtime.applications

import autospec.runtime.ConditionStatus.{Failed, Passed}
import autospec.runtime.exceptions.EndpointRequestFailureWithSymbols
import autospec.runtime.{ValidatedStreamFromRequestStream, _}
import autospec.schema.ApplicationSchema
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.client.asynchttpclient.AsyncHttpClient

class TestPlanConsoleApp()(implicit scheduler: Scheduler) {
  private val printer: SymbolPrinter = ScalaSymbolPrinter

  def run(schema: ApplicationSchema, paths: List[TestPlan], haltOnFailure: Boolean = true): Task[Unit] =
    AsyncHttpClient.resource[Task]().use { httpClient =>
      val httpRequestExecutor     = new HttpRequestExecutor(httpClient)
      val endpointRequestExecutor = new EndpointRequestExecutorImpl(httpRequestExecutor)
      val validationStream        = new ValidatedStreamFromRequestStream(endpointRequestExecutor)
      val testPathExecutor        = new TestPlanExecutor(validationStream)
      for {
        session     <- Session.newSession(schema)
        testResults <- testPathExecutor.executeMany(session, paths, haltOnFailure)
      } yield printResults(schema, paths, testResults)
    }

  private def printResults(
    schema: ApplicationSchema,
    paths: List[TestPlan],
    testResults: Map[TestPlanId, List[Either[EndpointRequestFailureWithSymbols, ValidatedRequestResponseWithSymbols]]]
  ): Unit = {
    def color(failed: Boolean): String = if (failed) Console.RED else Console.GREEN

    def printRequest(failed: Boolean, index: Int, request: EndpointRequestSymbolic): Unit =
      println(s"${color(failed)}    $index: ${printer.print(request, index)}")

    println(s"${color(false)}Tests:")
    println()

    paths.foreach { path =>
      println(s"${color(false)}  ${path.id.value}:")

      val pathResult         = testResults(path.id)
      val resolvedConditions = pathResult.flatMap(_.fold(_ => Map.empty, _.resolvedConditions)).toMap

      pathResult.zipWithIndex.foreach {
        case Left(failedRequest) -> index =>
          printRequest(true, index, failedRequest.request)

          println(
            s"${color(true)}       ▣ Aborted: request failed after several attempts."
          )

        case Right(validatedRequest) -> index =>
          val request          = validatedRequest.requestSymbolic
          val isTestPathFailed = validatedRequest.isFailed
          val schemaConditions = schema.endpoint(request.endpointId).conditions

          printRequest(isTestPathFailed, index, request)

          schemaConditions.foreach {
            case (conditionId, predicate) =>
              val (icon, color) =
                resolvedConditions.get(conditionId.withProvenance(validatedRequest.requestId)).map(_._1) match {
                  case None         => "?" -> Console.YELLOW
                  case Some(Failed) => "✖" -> Console.RED
                  case Some(Passed) => "✔" -> Console.GREEN
                }
              println(s"$color       $icon ${printer.print(predicate)}")
          }
      }

      print(Console.RESET)

      if (pathResult.exists(_.exists(_.isFailed))) {
        println(s"${Console.RESET}    responses:")
        pathResult
          .zipWithIndex
          .collect { case (Right(x), index) => s"      $index: ${x.response.toString}" }
          .foreach(println)
      }

      println()
    }

    val failureCount =
      testResults.values.count(testPath => testPath.exists(_.forall(_.isFailed)))

    if (failureCount === 0)
      print(s"${color(false)}All tests passed.")
    else
      print(s"${color(true)}Uh oh! You have $failureCount failed test${if (failureCount === 1) "" else "s"}.")

    println(Console.RESET)
  }

}
