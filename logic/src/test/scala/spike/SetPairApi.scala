package spike

import spike.SchemaSymbols._
import spike.macros.ClientMacros
import spike.runtime.EndpointRequest
import spike.schema._

/**
 * Holds two independent sets 'A' and 'B' and provides mutators/accessors to each.
 *
 * 'addA' has no impact on 'listB', and 'addB' has no impact on 'listA'.
 */
trait SetPairApi[A] {
  // State A
  def addA(value: Int): A
  def listA(): A

  // State B
  def addB(value: Int): A
  def listB(): A
}

object SetPairApi {
  object Schema extends SetPairApi[EndpointDefinition] {
    def addA(value: Int) =
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
            Endpoint(EndpointId("listA"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true),
            Parameter(EndpointParameterName("value"))
          ),
          Predicate.Equals(
            StatusCode, Literal(200)
          )
        )
      )

    def addB(value: Int) =
      EndpointDefinition(
        currentMethodEndpointId,
        testApiId,
        HttpMethod.Post,
        "/bars",
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
            Endpoint(EndpointId("listB"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true),
            Parameter(EndpointParameterName("value"))
          ),
          Predicate.Equals(
            StatusCode, Literal(200)
          )
        )
      )

    def listA() =
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

    def listB() =
      EndpointDefinition(
        currentMethodEndpointId,
        testApiId,
        HttpMethod.Get,
        "/bars",
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
  object Client extends SetPairApi[EndpointRequest] {
    implicit val schema: ApplicationSchema = schemaFromObject(Schema)
    def addA(value: Int) = ClientMacros.endpointRequest[EndpointRequest]()
    def addB(value: Int) = ClientMacros.endpointRequest[EndpointRequest]()
    def listA()          = ClientMacros.endpointRequest[EndpointRequest]()
    def listB()          = ClientMacros.endpointRequest[EndpointRequest]()
  }
}