package spike.runtime.applications

import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.client.asynchttpclient.AsyncHttpClient
import playground._
import spike.common.ULID
import spike.runtime.ConditionStatus.{Failed, Passed}
import spike.runtime.{EndpointRequestExecutorImpl, HttpRequestExecutor, ScalaSymbolPrinter, SymbolPrinter, ValidatedRequestResponse}
import spike.schema.ApplicationSchema

class GeneratorConsoleApp(implicit scheduler: Scheduler) {
  private val printer: SymbolPrinter = ScalaSymbolPrinter

  def run(schema: ApplicationSchema): Task[Unit] = {
    AsyncHttpClient.resource[Task]().use { httpClient =>
      val config = Config(1000)
      val httpRequestExecutor       = new HttpRequestExecutor(httpClient)
      val endpointRequestExecutor   = new EndpointRequestExecutorImpl(httpRequestExecutor)
      val requestResponseRepository = new RequestResponseRepository()
      val opportunitiesRepository   = new OpportunitiesRepository()
      val requestGenerator          = new RequestGenerator(requestResponseRepository, opportunitiesRepository, config)
      val validationStream          = new ValidationStreamFromGenerator(requestGenerator, endpointRequestExecutor, requestResponseRepository, opportunitiesRepository)
      for {
        sessionId <- ULID.next[Task].map(SessionId)
        session    = Session(sessionId, schema)
        _         <- validationStream(session).evalTap(processResult(schema)).compile.drain
      } yield ()
    }
  }

  private def processResult(schema: ApplicationSchema)(result: ValidatedRequestResponse): Task[Unit] =
    Task {
      def color(failed: Boolean) = if (failed) Console.RED else Console.GREEN

      println(s"${color(result.isFailed)}${result.request.prettyPrint}")

      result.resolvedConditions.foreach { case (condition, status) =>
        val predicate     = schema.condition(condition.conditionId)
        val (icon, color) = status match {
          case Failed => "✖" -> Console.RED
          case Passed => "✔" -> Console.GREEN
        }
        println(s"$color       $icon ${printer.print(predicate)}")
      }
    }
}
