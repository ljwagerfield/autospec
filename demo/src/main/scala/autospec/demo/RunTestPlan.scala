package autospec.demo

import cats.data.Chain
import cats.effect._
import cats.implicits._
import io.circe.Json
import monix.eval.Task
import monix.execution.Scheduler
import autospec.RuntimeSymbols
import autospec.demo.RestApiSchema._
import autospec.runtime.applications.TestPlanConsoleApp
import autospec.runtime.{EndpointRequestSymbolic, TestPlan, TestPlanId}
import autospec.schema._
import scala.collection.immutable.{Map => SMap}

object RunTestPlan extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    implicit val scheduler: Scheduler = Scheduler.traced

    val testPath = TestPlan(
      TestPlanId("example-test"),
      Chain(
        EndpointRequestSymbolic(
          EndpointId("list"),
          SMap.empty
        ),
        EndpointRequestSymbolic(
          EndpointId("delete"),
          SMap(
            EndpointParameterName("value") -> RuntimeSymbols.Literal(Json.fromInt(42))
          )
        ),
        EndpointRequestSymbolic(
          EndpointId("list"),
          SMap.empty
        ),
      )
    )

    Task.gather(
      List(
        new RestApi().run(),
        new TestPlanConsoleApp().run(schema, List(testPath)),
      )
    ).as(ExitCode.Success).to[IO]
  }
}
