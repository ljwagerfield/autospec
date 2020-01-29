package spike

import spike.SchemaSymbols._
import spike.schema.{ApplicationSchema, EndpointDefinition, EndpointId, EndpointParameter, EndpointParameterLocation}
import spike.runtime.TestPath

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

// Todo 1: have the below generate 'test plans'
// Todo 2: create something that executes 'test plans'
object App extends App {
  val schema = ApplicationSchema(List(
    EndpointDefinition(
      EndpointId("all"),
      "GET",
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
      "POST",
      "/foos",
      List(
        EndpointParameter(
          "value",
          EndpointParameterLocation.Body
        )
      ),
      Nil,
      List(
        Predicate.Contains(
          Endpoint(EndpointId("all"), scala.collection.Map.empty, evaluateAfterExecution = true),
          Parameter("value")
        )
      )
    )
  ))

  TestPath(schema).zipWithIndex.foreach { case (path, i) =>
    println()
    println(s"--- TEST PATH #${i + 1} ---")
    path.endpointCalls.foreach(println)
  }

  println(
    TestPath(schema)
  )
}