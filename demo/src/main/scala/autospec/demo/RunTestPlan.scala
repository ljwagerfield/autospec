package autospec.demo

import autospec.RuntimeSymbolsIndexed
import autospec.demo.RestApiSchema._
import autospec.runtime.applications.TestPlanConsoleApp
import autospec.runtime.{EndpointRequestSymbolic, TestPlan, TestPlanId}
import autospec.schema._
import cats.data.Chain
import io.circe.Json
import monix.eval.Task

import scala.collection.immutable.{Map => SMap}

object RunTestPlan extends RunWithApi {

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
          EndpointParameterName("value") -> RuntimeSymbolsIndexed.Literal(Json.fromInt(42))
        )
      ),
      EndpointRequestSymbolic(
        EndpointId("list"),
        SMap.empty
      )
    )
  )

  override protected def runAutoSpec(): Task[Unit] = new TestPlanConsoleApp().run(schema, List(testPath))
}
