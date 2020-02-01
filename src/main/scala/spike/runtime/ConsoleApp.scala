package spike.runtime

import cats.data.NonEmptyList
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.client.asynchttpclient.AsyncHttpClient
import spike.runtime.http.{HttpRequestEncoder, HttpRequestExecutor}
import spike.schema.{ApplicationSchema, ConditionId}

object ConsoleApp {
  def runTests(schema: ApplicationSchema): Unit = {
    implicit val scheduler: Scheduler = Scheduler.traced

    val task =
      AsyncHttpClient.resource[Task]().use { httpClient =>
        val httpRequestEncoder  = new HttpRequestEncoder()
        val httpRequestExecutor = new HttpRequestExecutor(httpClient)
        val testPathGenerator   = new TestPathGenerator()
        val testPlanExecutor    = new TestPlanExecutor(httpRequestEncoder, httpRequestExecutor)
        val testPlan            = TestPlan(schema, testPathGenerator)
        testPlanExecutor(testPlan).map { testResults =>
          printResults(testPlan, testResults)
        }
      }

    task.runSyncUnsafe()
  }

  private def printResults(testPlan: TestPlan, testResults: Map[EndpointRequestId, NonEmptyList[ConditionId]]): Unit = {
    testPlan.paths.zipWithIndex.foreach { case (path, i) =>
      println()
      println(s"--- TEST PATH #${i + 1} ---")
      path.requests.foreach(println)
    }

    val failures =
      for {
        failedRequest <- testResults.toList
        conditionId <- failedRequest._2.toList
      } yield {
        failedRequest._1 -> conditionId
      }

    if (failures.isEmpty)
      println("=== SUCCESS ===")
    else
      println(s"=== FAILURES: ${failures.size} ===")

    failures.zipWithIndex.foreach { case ((requestId, conditionId), i) =>
      println()
      println(s"--- FAILURE #${i + 1} ---")
      println(s"Trigger: path '${requestId.testPathId.value}' @ request #${requestId.requestIndex}")
      println(s"Failure: $conditionId")
    }
  }
}
