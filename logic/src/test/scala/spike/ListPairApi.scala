package spike

import spike.SchemaSymbols._
import spike.macros.ClientMacros
import spike.runtime.EndpointRequestSymbolic
import spike.schema._

trait ListPairApi[A] {
  def addA(value: Int): A
  def removeA(value: Int): A
  def listA(): A

  def addB(value: Int): A
  def removeB(value: Int): A
  def listB(): A
}

object ListPairApi {
  object Schema extends ListPairApi[EndpointDefinition] {
    def addA(value: Int) =
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
          Predicate.Equals(
            // IMPORTANT: do not change the order of this equals! It's required to be this way for the following test:
            // "treat endpoints that have a postcondition that contains both an resolvable reverse lookup and an unresolvable forward lookup as mutating"
            Concat(
              Endpoint(EndpointId("listA"), scala.collection.immutable.Map.empty, evaluateAfterExecution = false),
              Parameter(EndpointParameterName("value"))
            ),
            Endpoint(EndpointId("listA"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true)
          ),
          Predicate.Equals(
            StatusCode, Literal(200)
          )
        )
      )

    def removeA(value: Int) =
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
              Endpoint(EndpointId("listA"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true),
              Parameter(EndpointParameterName("value"))
            )
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
            Endpoint(currentMethodEndpointId, scala.collection.immutable.Map.empty, evaluateAfterExecution = false),
            ResponseBody
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
            EndpointParameterType.Int32,
            EndpointParameterLocation.Body,
            EndpointParameterSerialization.ToString("text/plain")
          )
        ),
        Nil,
        List(
          Predicate.Equals(
            // IMPORTANT: do not change the order of this equals! It's required to be this way for the following test:
            // "treat endpoints that have a postcondition that contains both an resolvable reverse lookup and an unresolvable forward lookup as mutating"
            Concat(
              Endpoint(EndpointId("listB"), scala.collection.immutable.Map.empty, evaluateAfterExecution = false),
              Parameter(EndpointParameterName("value"))
            ),
            Endpoint(EndpointId("listB"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true)
          ),
          Predicate.Equals(
            StatusCode, Literal(200)
          )
        )
      )

    def removeB(value: Int) =
      EndpointDefinition(
        currentMethodEndpointId,
        testApiId,
        HttpMethod.Post,
        "/bars",
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
              Endpoint(EndpointId("listB"), scala.collection.immutable.Map.empty, evaluateAfterExecution = true),
              Parameter(EndpointParameterName("value"))
            )
          ),
          Predicate.Equals(
            StatusCode, Literal(200)
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
            Endpoint(currentMethodEndpointId, scala.collection.immutable.Map.empty, evaluateAfterExecution = false),
            ResponseBody
          )
        )
      )
  }
  object Client extends ListPairApi[EndpointRequestSymbolic] {
    implicit val schema: ApplicationSchema = schemaFromObject(Schema)
    def addA(value: Int)    = ClientMacros.endpointRequest()
    def removeA(value: Int) = ClientMacros.endpointRequest()
    def listA()             = ClientMacros.endpointRequest()

    def addB(value: Int)    = ClientMacros.endpointRequest()
    def removeB(value: Int) = ClientMacros.endpointRequest()
    def listB()             = ClientMacros.endpointRequest()
  }
}
