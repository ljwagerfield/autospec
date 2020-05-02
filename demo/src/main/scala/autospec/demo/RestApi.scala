package autospec.demo

import cats.effect._
import cats.effect.concurrent.Ref
import com.github.ghik.silencer.silent
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

@silent
class RestApi()(implicit scheduler: Scheduler) extends Http4sDsl[Task] {
  private var state               = Map.empty[String, String]
  implicit val timer: Timer[Task] = Task.timer(scheduler)

  val myService: HttpRoutes[Task] = HttpRoutes.of[Task] {
    // "list"
    case GET -> Root / "foos" =>
      Ok(state.asJson)

    // "get"
    case GET -> Root / "foos" / key =>
      state.get(key).fold(NotFound())(value => Ok(value.asJson))

    // "delete"
    case DELETE -> Root / "foos" / key =>
      state = state - key
      NoContent()

    // "set"
    case body @ POST -> Root / "foos" / key =>
      {
        for {
          value <- body.as[String]
          _     <- Task { state = state ++ Map(key -> value) }
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
