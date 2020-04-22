package autospec

import autospec.SchemaSymbols._
import autospec.macros.ClientMacros
import autospec.runtime.EndpointRequestSymbolic
import autospec.schema._

trait ListApi[A] {
  def add(value: Int): A
  def remove(value: Int): A
  def list(): A
}

object ListApi {
  object Schema extends ListApi[EndpointDefinition] {
    def add(value: Int) =
      EndpointDefinition(
        currentMethodEndpointId,
        testApiId,
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
          // IMPORTANT: Some tests rely on this condition performing BOTH a reverse and forward lookup, so do not change.
          Predicate.Equals(
            Concat(
              Endpoint(EndpointId("list"), scala.collection.immutable.Map.empty, evaluateAfterExecution = false),
              Parameter(EndpointParameterName("value"))
            ),
            Endpoint(EndpointId("list"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true)
          ),
          Predicate.Equals(
            StatusCode, Literal(200)
          )
        )
      )

    def remove(value: Int) =
      EndpointDefinition(
        currentMethodEndpointId,
        testApiId,
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
          Predicate.Not(
            Predicate.Contains(
              Endpoint(EndpointId("list"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true),
              Parameter(EndpointParameterName("value"))
            )
          ),
          Predicate.Equals(
            StatusCode, Literal(200)
          )
        )
      )

    def list() =
      EndpointDefinition(
        currentMethodEndpointId,
        testApiId,
        HttpMethod.Get,
        "/foos",
        Nil,
        Nil,
        List(
          Predicate.Equals(
            Endpoint(currentMethodEndpointId, scala.collection.immutable.Map.empty, evaluateAfterExecution = false),
            ResponseBody
          )
        )
      )
  }
  object Client extends ListApi[EndpointRequestSymbolic] {
    implicit val schema: ApplicationSchema = schemaFromObject(Schema)
    def add(value: Int)    = ClientMacros.endpointRequest()
    def remove(value: Int) = ClientMacros.endpointRequest()
    def list()             = ClientMacros.endpointRequest()
  }
}
