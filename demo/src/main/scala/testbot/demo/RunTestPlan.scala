package testbot.demo

import cats.data.Chain
import cats.effect._
import cats.implicits._
import io.circe.Json
import monix.eval.Task
import monix.execution.Scheduler
import testbot.RuntimeSymbols
import testbot.demo.RestApiSchema._
import testbot.runtime.applications.TestPlanConsoleApp
import testbot.runtime.{EndpointRequestSymbolic, TestPlan, TestPlanId}
import testbot.schema._
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
