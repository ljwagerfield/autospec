package spike

import io.circe.Json
import spike.SchemaSymbols._
import spike.runtime.EndpointRequest
import spike.schema._

trait SetApi[A] {
  def add(value: Int): A
  def list(): A
}

object SetApi {
  object Client extends SetApi[EndpointRequest] {
    implicit val schema: ApplicationSchema = schemaFromObject(Schema)

    def add(value: Int) =
      EndpointRequest(
        currentMethodEndpointId,
        scala.collection.immutable.Map(
          EndpointParameterName("value") -> RuntimeSymbols.Literal(Json.fromInt(value))
        )
      )

    def list() =
      EndpointRequest(
        currentMethodEndpointId,
        scala.collection.immutable.Map.empty
      )
  }

  object Schema extends SetApi[EndpointDefinition] {
    def add(value: Int) =
      EndpointDefinition(
        currentMethodEndpointId,
        testApiId,
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
            Endpoint(EndpointId("list"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true),
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
            Count(Distinct(ResponseBody)),
            Count(ResponseBody)
          ),
          Predicate.Equals(
            Endpoint(currentMethodEndpointId, scala.collection.immutable.Map.empty, evaluateAfterExecution = false),
            ResponseBody
          )
        )
      )
  }
}
