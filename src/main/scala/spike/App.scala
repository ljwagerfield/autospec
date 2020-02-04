package spike

import spike.SchemaSymbols._
import spike.runtime.ConsoleApp
import spike.schema._

class SetController {
  private var state = List.empty[Int]

  // GET /foos
  def all: List[Int] =
    state

  // POST /foos
  // 42
  def add(value: Int): Unit =
    state = value :: state

//  // DELETE /foos/:value
//  def remove(value: Int): Unit =
//    state = state.filterNot(_ == value)
//
//  // GET /foos/:value/exists
//  def exists(value: Int): Boolean =
//    state.contains(value)
}

object App extends App {
  val apiId = ApiId("api")
  val schema = ApplicationSchema(
    List(
      ApiDefinition(
        apiId,
        "https://localhost:9000"
      )
    ),
    List(
      EndpointDefinition(
        EndpointId("all"),
        apiId,
        HttpMethod.Get,
        "/foos",
        Nil,
        Nil,
        List(
          Predicate.Equals(
            Count(Distinct(Result)),
            Count(Result)
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
            EndpointParameterLocation.Body,
            EndpointParameterSerialization.ToString("text/plain")
          )
        ),
        Nil,
        List(
          Predicate.Contains(
            Endpoint(EndpointId("all"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true),
            Parameter(EndpointParameterName("value"))
          )
        )
      )
    )
  )

  ConsoleApp.runTests(schema)
}