package spike.runtime

import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.client.asynchttpclient.AsyncHttpClient
import spike.common.ULIDFactory
import spike.runtime.ConditionStatus.{Failed, Passed, Unresolvable}
import spike.runtime.TestPathExecutor.ValidatedRequestResponse
import spike.schema.ApplicationSchema

class ConsoleApp()(implicit scheduler: Scheduler) {
  private val printer: SymbolPrinter = ScalaSymbolPrinter

  def run(schema: ApplicationSchema, paths: List[TestPathOld]): Task[Unit] =
    AsyncHttpClient.resource[Task]().use { httpClient =>
      val ulidFactory             = new ULIDFactory
      val httpRequestExecutor     = new HttpRequestExecutor(httpClient)
      val endpointRequestExecutor = new EndpointRequestExecutor(httpRequestExecutor, ulidFactory)
      val testPathExecutor        = new TestPathExecutor(endpointRequestExecutor)
      testPathExecutor.execute(schema, paths).map { testResults =>
        printResults(schema, paths, testResults)
      }
    }

  private def printResults(schema: ApplicationSchema, paths: List[TestPathOld], testResults: Map[TestPathId, List[ValidatedRequestResponse]]): Unit = {
    def color(failed: Boolean) = if (failed) Console.RED else Console.GREEN

    println(s"${color(false)}Tests:")
    println()

    paths.foreach { path =>
      println(s"${color(false)}  ${path.id.value}:")
      val pathResult = testResults(path.id)
      pathResult.zipWithIndex.foreach { case (result, index) =>
        val request          = result.requestSymbolic
        val isTestPathFailed = result.isFailed
        val conditions       = schema.endpoint(request.endpointId).conditions
        val status           = result.allConditions(schema)

        println(s"${color(isTestPathFailed)}    $index: ${printer.print(request, index)}")
        conditions.foreach { case (conditionId, predicate) =>
          val (icon, color)   = status(conditionId) match {
            case Unresolvable => "?" -> Console.YELLOW
            case Failed       => "✖" -> Console.GREEN
            case Passed       => "✔" -> Console.RED
          }
          println(s"$color       $icon ${printer.print(predicate)}")
        }
      }

      print(Console.RESET)

      if (pathResult.exists(_.isFailed)) {
        println(s"${Console.RESET}    responses:")
        pathResult.zipWithIndex.foreach { case (response, index) =>
          println(s"      $index: $response")
        }
      }

      println()
    }

    val failureCount = testResults.values.toList.flatMap(_.toList).flatMap(_.resolvedConditions.values.toList).count(_.isFailed)

    if (failureCount === 0)
      print(s"${color(false)}All tests passed.")
    else
      print(s"${color(true)}Uh oh! You have $failureCount failed condition${if (failureCount === 1) "" else "s"}.")

    println(Console.RESET)
  }
}
