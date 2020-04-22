package autospec.runtime.applications

import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.client.asynchttpclient.AsyncHttpClient
import playground._
import autospec.common.ULID
import autospec.runtime.ConditionStatus.{Failed, Passed}
import autospec.runtime.{EndpointRequestExecutorImpl, EndpointRequestId, HttpRequestExecutor, ScalaSymbolPrinter, SymbolPrinter, ValidatedRequestResponse}
import autospec.schema.ApplicationSchema
import cats.implicits._
import autospec.runtime.applications.GeneratorConsoleApp.State

import scala.collection.immutable.Queue

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
        _         <- validationStream(session)
                       .evalMapAccumulate(State.initial)((s, r) => processResult(schema, r, s).map(_ -> ()))
                       .compile
                       .drain
      } yield ()
    }
  }

  private def processResult(schema: ApplicationSchema, result: ValidatedRequestResponse, state: State): Task[State] =
    Task {
      def color(failed: Boolean) = if (failed) Console.RED else Console.GREEN

      println(s"${color(result.isFailed)}#${state.requestIndex} ${result.request.prettyPrint}")

      result.resolvedConditions.foreach { case (condition, status) =>
        val predicate     = schema.condition(condition.conditionId)
        val provenance    =
          if (condition.provenance === result.requestId)
            None
          else
            Some(state.resolve(condition.provenance).fold("[from earlier request]")(idx => s"[from request #$idx]"))

        val (icon, color) = status match {
          case Failed => "✖" -> Console.RED
          case Passed => "✔" -> Console.GREEN
        }
        println(s"$color   $icon ${printer.print(predicate)} ${provenance.getOrElse("")}")
      }

      state.add(result.requestId)
    }
}

object GeneratorConsoleApp {
  private val requestHistorySize = 1000 // The maximum depth we expect for a deferred condition (i.e. consecutive pure calls)

  private case class State(requestIndex: Long, recentRequests: Queue[(EndpointRequestId, Long)]) {
    def add(request: EndpointRequestId): State = {
      val newRecentRequests = recentRequests.enqueue(request -> requestIndex)
      copy(
        requestIndex = requestIndex + 1,
        recentRequests =
          if (recentRequests.size === requestHistorySize + 1)
            newRecentRequests.dequeue._2
          else
            newRecentRequests
      )
    }

    def resolve(request: EndpointRequestId): Option[Long] =
      recentRequests.findLast(_._1 === request).map(_._2)
  }

  private object State {
    val initial: State = State(1, Queue.empty)
  }
}
