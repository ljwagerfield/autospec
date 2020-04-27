package autospec.demo

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._
import fs2.concurrent.Signal
import io.circe.syntax._
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder

import scala.util.Random

class RestApi()(implicit scheduler: Scheduler) extends Http4sDsl[Task] {
  private var state               = List.empty[Int]
  implicit val timer: Timer[Task] = Task.timer(scheduler)

  val myService: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case GET -> Root / "foos" =>
      Ok(state.asJson)

    case DELETE -> Root / "foos" / value =>
      val valueInt = value.toInt
      state = state.filterNot(_ === valueInt)
      if (Random.nextInt(2) == 0)
        RequestTimeout()
      else
        NoContent()

    case body @ POST -> Root / "foos" =>
      {
        for {
          value <- body.as[String]
          _     <- Task { state = (value.toInt :: state).distinct }
          resp  <- Ok(())
        } yield resp
      }.onErrorRecoverWith {
        case t => InternalServerError(t.toString)
      }

  }

  def run(exitSignal: Signal[Task, Boolean]): Task[Unit] = {
    val services = myService // <+> fooService <+> barService

    val httpApp = Router("/" -> services).orNotFound

    BlazeServerBuilder[Task]
      .bindHttp(9005, "localhost")
      .withHttpApp(httpApp)
      .withNio2(true) // Reduces errors on process termination.
      .serveWhile(exitSignal, Ref.unsafe(ExitCode.Success))
      .compile
      .drain
  }

}
