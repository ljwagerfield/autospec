package spike.runtime

import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.client.asynchttpclient.AsyncHttpClient
import spike.runtime.http.{HttpRequestEncoder, HttpRequestExecutor}
import spike.schema.ApplicationSchema

class ConsoleApp()(implicit scheduler: Scheduler) {
  def run(schema: ApplicationSchema): Task[Unit] =
    run(TestPlan.from(schema, new TestPathGenerator()))

  def run(schema: ApplicationSchema, paths: List[TestPath]): Task[Unit] =
    run(TestPlan.from(schema, paths))

  private def run(testPlan: TestPlan): Task[Unit] =
    AsyncHttpClient.resource[Task]().use { httpClient =>
      val httpRequestEncoder  = new HttpRequestEncoder()
      val httpRequestExecutor = new HttpRequestExecutor(httpClient)
      val testPlanExecutor    = new TestPlanExecutor(httpRequestEncoder, httpRequestExecutor)
      testPlanExecutor(testPlan).map { testResults =>
        printResults(testPlan, testResults)
      }
    }

  private def printResults(testPlan: TestPlan, testResults: Map[EndpointRequestId, FailedTestPath]): Unit = {
    println(s"TESTS (${testPlan.paths.size})")

    testPlan.paths.foreach { path =>
      println(s"${path.id.value}:")
      path.requests.zipWithIndex.foreach { case (request, i) =>
        println(s"  | $i | $request")
      }
      println()
    }

    val failures     = testResults.values
    val failureCount = testResults.values.toList.foldMap(_.failures.size)

    if (failureCount === 0)
      println("SUCCESS")
    else
      println(s"FAILURES ($failureCount)")

    failures.foreach { failure =>
      println(s"${failure.testPathId.value} (stopped at request #${failure.lastRequestIndex}):")
      println(s"  failed conditions:")
      failure.failures.toList.zipWithIndex.foreach { case (additionalConditions, index) =>
        println(s"  | $index | $additionalConditions")
      }
      println(s"  responses:")
      failure.responses.toList.zipWithIndex.foreach { case (response, index) =>
        println(s"  | $index | $response")
      }
      println()
    }
  }
}
