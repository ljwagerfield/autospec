package autospec.runtime.applications

import autospec.runtime.ConditionStatus.{Failed, Passed}
import autospec.runtime._
import autospec.runtime.exceptions.EndpointRequestFailure
import autospec.schema.ApplicationSchema
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.client.asynchttpclient.AsyncHttpClient

class GeneratorConsoleApp(implicit scheduler: Scheduler) {
  private val printer: SymbolPrinter = ScalaSymbolPrinter

  def run(schema: ApplicationSchema): Task[Unit] =
    AsyncHttpClient.resource[Task]().use { httpClient =>
      val httpRequestExecutor     = new HttpRequestExecutor(httpClient)
      val endpointRequestExecutor = new EndpointRequestExecutorImpl(httpRequestExecutor)
      val requestGenerator        = new RequestGenerator(endpointRequestExecutor)
      val validationStream        = new ValidatedStreamFromGenerator(requestGenerator)
      for {
        session <- Session.newSession(schema)
        _ <- validationStream(session)
               .evalTap(processResult(schema, _))
               .compile
               .drain
      } yield ()
    }

  private def processResult(
    schema: ApplicationSchema,
    result: Either[EndpointRequestFailure, ValidatedRequestResponse]
  ): Task[Unit] =
    Task {
      def color(failed: Boolean) = if (failed) Console.RED else Console.GREEN
      val isFailed               = result.forall(_.isFailed)
      val requestId              = result.fold(_.requestId, _.requestId)
      val request                = result.fold(_.request, _.request)

      println(s"${color(isFailed)}#${requestId.requestIndex.index} ${request.prettyPrint}")

      result.fold(
        _ => println(s"${Console.RED}   ⃠ request failed after several retry attempts"),
        _.resolvedConditions.foreach {
          case (condition, status) =>
            val predicate = schema.condition(condition.conditionId)
            val provenance =
              if (condition.provenance === requestId)
                None
              else
                Some(s"[from request #${condition.provenance.requestIndex.index}]")

            val (icon, color) = status._1 match {
              case Failed => "✖" -> Console.RED
              case Passed => "✔" -> Console.GREEN
            }
            println(s"$color   $icon ${printer.print(predicate)} ${provenance.getOrElse("")}")
        }
      )
    }

}
