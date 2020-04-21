package spike.demo

import cats.effect._
import cats.implicits._
import io.circe.syntax._
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import spike.SchemaSymbols._
import spike.runtime.applications.GeneratorConsoleApp
import spike.schema._

class SetController()(implicit scheduler: Scheduler) extends Http4sDsl[Task] {
  private var state = List.empty[Int]
  implicit val timer: Timer[Task] = Task.timer(scheduler)

  val myService: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case GET -> Root / "foos" =>
      Ok(state.asJson)

    case body @ POST -> Root / "foos" =>
      {
        for {
          value <- body.as[String]
          _     <- Task { state = (value.toInt :: state).distinct }
          resp  <- Ok(())
        } yield {
          resp
        }
      }.onErrorRecoverWith {
        case t => InternalServerError(t.toString)
      }

  }

  def run(): Task[Unit] = {
    val services = myService // <+> fooService <+> barService

    val httpApp = Router("/" -> services).orNotFound

    BlazeServerBuilder[Task]
      .bindHttp(9005, "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
  }
}

object App extends IOApp {
  val apiId = ApiId("api")
  val schema = ApplicationSchema(
    List(
      ApiDefinition(
        apiId,
        "http://localhost:9005"
      )
    ),
    List(
      EndpointDefinition(
        EndpointId("list"),
        apiId,
        HttpMethod.Get,
        "/foos",
        Nil,
        Nil,
        List(
          Predicate.Equals(
            Count(Distinct(ResponseBody)),
            Count(ResponseBody)
          ),
          Predicate.Equals(
            Endpoint(EndpointId("list"), scala.collection.immutable.Map.empty, evaluateAfterExecution = false),
            ResponseBody
          )
        )
      ),
      EndpointDefinition(
        EndpointId("add"),
        apiId,
        HttpMethod.Post,
        "/foos",
        List(
          EndpointParameter(
            EndpointParameterName("value"),
            EndpointParameterType.Int32,
            EndpointParameterLocation.Body,
            EndpointParameterSerialization.ToString("text/plain")
          )
        ),
        Nil,
        List(
          Predicate.Contains(
            Endpoint(EndpointId("list"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true),
            Parameter(EndpointParameterName("value"))
          ),
          Predicate.Equals(
            StatusCode, Literal(200)
          )
        )
      )
    )
  )

  def run(args: List[String]): IO[ExitCode] = {
    implicit val scheduler: Scheduler = Scheduler.traced

//    val testPath = TestPlan(
//      TestPlanId("example-test"),
//      Chain(
//        EndpointRequestSymbolic(
//          EndpointId("add"),
//          scala.collection.immutable.Map(
//            EndpointParameterName("value") -> RuntimeSymbols.Literal(Json.fromInt(42))
//          )
//        ),
//        EndpointRequestSymbolic(
//          EndpointId("list"),
//          scala.collection.immutable.Map.empty
//        ),
//        EndpointRequestSymbolic(
//          EndpointId("list"),
//          scala.collection.immutable.Map.empty
//        ),
//        EndpointRequestSymbolic(
//          EndpointId("add"),
//          scala.collection.immutable.Map(
//            EndpointParameterName("value") -> RuntimeSymbols.Literal(Json.fromInt(42))
//          )
//        ),
//        EndpointRequestSymbolic(
//          EndpointId("add"),
//          scala.collection.immutable.Map(
//            EndpointParameterName("value") -> RuntimeSymbols.Literal(Json.fromInt(52))
//          )
//        ),
//        EndpointRequestSymbolic(
//          EndpointId("list"),
//          scala.collection.immutable.Map.empty
//        )
//      )
//    )

    Task.gather(
      List(
        new SetController().run(),
//        new TestPlanConsoleApp().run(schema, List(testPath)),
        new GeneratorConsoleApp().run(schema)
      )
    ).as(ExitCode.Success).to[IO]
  }
}