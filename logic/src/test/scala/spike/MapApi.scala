package spike

import spike.SchemaSymbols._
import spike.macros.ClientMacros
import spike.runtime.EndpointRequestOld
import spike.schema._

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
            EndpointParameterLocation.Querystring,
            EndpointParameterSerialization.ToString("text/plain")
          ),
          EndpointParameter(
            EndpointParameterName("value"),
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
  object Client extends MapApi[EndpointRequestOld] {
    implicit val schema: ApplicationSchema = schemaFromObject(Schema)
    def set(key: String, value: Int) = ClientMacros.endpointRequest()
    def list()                       = ClientMacros.endpointRequest()
  }
}
