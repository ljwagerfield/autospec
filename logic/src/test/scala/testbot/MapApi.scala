package autospec

import autospec.SchemaSymbols._
import autospec.macros.ClientMacros
import autospec.runtime.EndpointRequestSymbolic
import autospec.schema._

trait MapApi[A] {
  def set(key: String, value: Int): A
  def list(): A
}

object MapApi {
  object Schema extends MapApi[EndpointDefinition] {
    def set(key: String, value: Int) =
      EndpointDefinition(
        currentMethodEndpointId,
        testApiId,
        HttpMethod.Post,
        "/foos",
        List(
          EndpointParameter(
            EndpointParameterName("key"),
            EndpointParameterType.String,
            EndpointParameterLocation.Querystring,
            EndpointParameterSerialization.ToString("text/plain")
          ),
          EndpointParameter(
            EndpointParameterName("value"),
            EndpointParameterType.Int32,
            EndpointParameterLocation.Querystring,
            EndpointParameterSerialization.ToString("text/plain")
          )
        ),
        Nil,
        List(
          Predicate.Equals(
            Map(
              Endpoint(EndpointId("list"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true),
              Parameter(EndpointParameterName("key"))
            ),
            Parameter(EndpointParameterName("value"))
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
  object Client extends MapApi[EndpointRequestSymbolic] {
    implicit val schema: ApplicationSchema = schemaFromObject(Schema)
    def set(key: String, value: Int) = ClientMacros.endpointRequest()
    def list()                       = ClientMacros.endpointRequest()
  }
}
