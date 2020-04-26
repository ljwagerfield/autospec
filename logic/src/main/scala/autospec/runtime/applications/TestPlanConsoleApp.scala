package autospec.runtime.applications

import autospec.runtime.ConditionStatus.{Failed, Passed}
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.client.asynchttpclient.AsyncHttpClient
import playground.{Session, ValidationStreamFromTestPlan}
import autospec.runtime._
import autospec.schema.ApplicationSchema

class TestPlanConsoleApp()(implicit scheduler: Scheduler) {
  private val printer: SymbolPrinter = ScalaSymbolPrinter

  def run(schema: ApplicationSchema, paths: List[TestPlan]): Task[Unit] =
    AsyncHttpClient.resource[Task]().use { httpClient =>
      val httpRequestExecutor     = new HttpRequestExecutor(httpClient)
      val endpointRequestExecutor = new EndpointRequestExecutorImpl(httpRequestExecutor)
      val validationStream        = new ValidationStreamFromTestPlan(endpointRequestExecutor)
      val testPathExecutor        = new TestPlanExecutor(validationStream)
      for {
        session     <- Session.newSession(schema)
        testResults <- testPathExecutor.executeMany(session, paths, haltOnFailure = true)
      } yield printResults(schema, paths, testResults)
    }

  private def printResults(
    schema: ApplicationSchema,
    paths: List[TestPlan],
    testResults: Map[TestPlanId, List[ValidatedRequestResponseWithSymbols]]
  ): Unit = {
    def color(failed: Boolean) = if (failed) Console.RED else Console.GREEN

    println(s"${color(false)}Tests:")
    println()

    paths.foreach { path =>
      println(s"${color(false)}  ${path.id.value}:")
      val pathResult    = testResults(path.id)
      val allConditions = pathResult.flatMap(_.resolvedConditions).toMap
      pathResult.foreach { result =>
        val index            = result.requestId.requestIndex.index
        val request          = result.requestSymbolic
        val isTestPathFailed = result.isFailed
        val conditions       = schema.endpoint(request.endpointId).conditions

        println(s"${color(isTestPathFailed)}    $index: ${printer.print(request, index)}")
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

      println()
    }

    val failureCount =
      testResults.values.toList.flatMap(_.toList).flatMap(_.resolvedConditions.values.toList).count(_._1.isFailed)

    if (failureCount === 0)
      print(s"${color(false)}All tests passed.")
    else
      print(s"${color(true)}Uh oh! You have $failureCount failed condition${if (failureCount === 1) "" else "s"}.")

    println(Console.RESET)
  }

}
