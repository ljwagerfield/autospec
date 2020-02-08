package spike.runtime

import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.client.asynchttpclient.AsyncHttpClient
import spike.runtime.http.{HttpRequestEncoder, HttpRequestExecutor}
import spike.schema.ApplicationSchema

class ConsoleApp()(implicit scheduler: Scheduler) {
  private val printer: SymbolPrinter               = ScalaSymbolPrinter
  private val testPathGenerator: TestPathGenerator = new TestPathGenerator()
  private val testPlanGenerator: TestPlanGenerator = new TestPlanGenerator()

  def run(schema: ApplicationSchema): Task[Unit] =
    run(testPlanGenerator.generate(schema, testPathGenerator.generate(schema)))

  def run(schema: ApplicationSchema, paths: List[TestPath]): Task[Unit] =
    run(testPlanGenerator.generate(schema, paths))

  private def run(testPlan: TestPlan): Task[Unit] =
    AsyncHttpClient.resource[Task]().use { httpClient =>
      val httpRequestEncoder  = new HttpRequestEncoder()
      val httpRequestExecutor = new HttpRequestExecutor(httpClient)
      val testPlanExecutor    = new TestPlanExecutor(httpRequestEncoder, httpRequestExecutor)
      testPlanExecutor.execute(testPlan).map { testResults =>
        printResults(testPlan, testResults)
      }
    }

  private def printResults(testPlan: TestPlan, testResults: Map[EndpointRequestId, FailedTestPath]): Unit = {
    def color(failed: Boolean) = if (failed) Console.RED else Console.GREEN

    println(s"${color(false)}Tests:")
    println()

    testPlan.paths.foreach { path =>
      println(s"${color(false)}  ${path.id.value}:")
      path.requests.zipWithIndex.foreach { case (EndpointRequestWithChecks(request, checks), i) =>
        val requestId = EndpointRequestId(path.id, i)
        val failure   = testResults.get(requestId)
        val failedConditions = failure.toList.flatMap(_.failures.toList).toSet
        val isTestPathFailed = failure.nonEmpty

        println(s"${color(isTestPathFailed)}    $i: ${printer.print(request, i)}")
        checks.foreach { case (conditionId, predicate) =>
          val failed = failedConditions.contains(conditionId)
          println(s"${color(failedConditions.contains(conditionId))}       ${if (failed) "✖" else "✔"} ${printer.print(predicate, i)}")
        }
      }

      print(Console.RESET)

      testResults.find(_._1.testPathId === path.id).foreach { case (_, failure) =>
        println(s"${Console.RESET}    responses:")
        failure.responses.toList.zipWithIndex.foreach { case (response, index) =>
          println(s"      $index: $response")
        }
      }

      println()
    }

    val failureCount = testResults.values.toList.foldMap(_.failures.size)

    if (failureCount === 0)
      println(s"${color(false)}All tests passed.")
    else
      println(s"${color(true)}Uh oh! You have $failureCount failed condition${if (failureCount === 1) "" else "s"}.")
  }
}
