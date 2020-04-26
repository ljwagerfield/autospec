package autospec.runtime.applications

import autospec.runtime.ConditionStatus.{Failed, Passed}
import autospec.runtime._
import autospec.schema.ApplicationSchema
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.client.asynchttpclient.AsyncHttpClient
import playground._

class GeneratorConsoleApp(implicit scheduler: Scheduler) {
  private val printer: SymbolPrinter = ScalaSymbolPrinter

  def run(schema: ApplicationSchema): Task[Unit] =
    AsyncHttpClient.resource[Task]().use { httpClient =>
      val config                    = Config(1000)
      val httpRequestExecutor       = new HttpRequestExecutor(httpClient)
      val endpointRequestExecutor   = new EndpointRequestExecutorImpl(httpRequestExecutor)
      val requestResponseRepository = new RequestResponseRepository()
      val opportunitiesRepository   = new OpportunitiesRepository()
      val requestGenerator          = new RequestGenerator(requestResponseRepository, opportunitiesRepository, config)
      val validationStream = new ValidationStreamFromGenerator(
        requestGenerator,
        endpointRequestExecutor,
        requestResponseRepository,
        opportunitiesRepository
      )
      for {
        session <- Session.newSession(schema)
        _ <- validationStream(session)
          .evalTap(processResult(schema, _))
          .compile
          .drain
      } yield ()
    }

  private def processResult(schema: ApplicationSchema, result: ValidatedRequestResponse): Task[Unit] =
    Task {
      def color(failed: Boolean) = if (failed) Console.RED else Console.GREEN

      println(s"${color(result.isFailed)}#${result.requestId.requestIndex.index} ${result.request.prettyPrint}")

      result.resolvedConditions.foreach {
        case (condition, status) =>
          val predicate = schema.condition(condition.conditionId)
          val provenance =
            if (condition.provenance === result.requestId)
              None
            else
              Some(s"[from request #${condition.provenance.requestIndex.index}]")

          val (icon, color) = status._1 match {
            case Failed => "✖" -> Console.RED
            case Passed => "✔" -> Console.GREEN
          }
          println(s"$color   $icon ${printer.print(predicate)} ${provenance.getOrElse("")}")
      }
    }

}
