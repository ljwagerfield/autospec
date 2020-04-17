package spike

import spike.SchemaSymbols._
import spike.macros.ClientMacros
import spike.runtime.EndpointRequestSymbolic
import spike.schema._

trait SetApi[A] {
  def add(value: Int): A
  def remove(value: Int): A
  def removeOrError(value: Int): A
  def list(): A
  def count(): A
}

object SetApi {
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

    def remove(value: Int) =
      EndpointDefinition(
        currentMethodEndpointId,
        testApiId,
        HttpMethod.Delete,
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

    def removeOrError(value: Int) =
      EndpointDefinition(
        currentMethodEndpointId,
        testApiId,
        HttpMethod.Delete,
        "/foos",
        List(
          EndpointParameter(
            EndpointParameterName("value"),
            EndpointParameterType.Int32,
            EndpointParameterLocation.Body,
            EndpointParameterSerialization.ToString("text/plain")
          )
        ),
        List(
          Precondition(
            Predicate.Contains(
              Endpoint(EndpointId("list"), scala.collection.immutable.Map.empty, evaluateAfterExecution = false),
              Parameter(EndpointParameterName("value"))
            ),
            404
          )
        ),
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
            Count(Distinct(ResponseBody)),
            Count(ResponseBody)
          ),
          Predicate.Equals(
            Endpoint(currentMethodEndpointId, scala.collection.immutable.Map.empty, evaluateAfterExecution = false),
            ResponseBody
          )
        )
      )

    def count() =
      EndpointDefinition(
        currentMethodEndpointId,
        testApiId,
        HttpMethod.Get,
        "/foos/count",
        Nil,
        Nil,
        List(
          Predicate.Equals(
            Count(Endpoint(EndpointId("list"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true)),
            ResponseBody
          )
        ),
        forcePure = true
      )
  }
  object Client extends SetApi[EndpointRequestSymbolic] {
    implicit val schema: ApplicationSchema = schemaFromObject(Schema)
    def add(value: Int)           = ClientMacros.endpointRequest()
    def remove(value: Int)        = ClientMacros.endpointRequest()
    def removeOrError(value: Int) = ClientMacros.endpointRequest()
    def list()                    = ClientMacros.endpointRequest()
    def count()                   = ClientMacros.endpointRequest()
  }
}
