package testbot.demo

import testbot.SchemaSymbols._
import testbot.schema._
import scala.collection.immutable.{Map => SMap}

object RestApiSchema {
  val apiId: ApiId              = ApiId("api")
  val schema: ApplicationSchema = ApplicationSchema(
    ApiDefinition(apiId, "http://localhost:9005") :: Nil,
    EndpointDefinition(
      EndpointId("list"),
      apiId,
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
          Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = false),
          ResponseBody
        )
      )
    ) ::
      EndpointDefinition(
        EndpointId("add"),
        apiId,
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
            Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = true),
            Parameter(EndpointParameterName("value"))
          ),
          Predicate.Equals(
            StatusCode, Literal(200)
          )
        )
      ) ::
      EndpointDefinition(
        EndpointId("delete"),
        apiId,
        HttpMethod.Delete,
        "/foos/:value",
        List(
          EndpointParameter(
            EndpointParameterName("value"),
            EndpointParameterType.Int32,
            EndpointParameterLocation.Path,
            EndpointParameterSerialization.ToString("text/plain")
          )
        ),
        Nil,
        List(
          Predicate.Not(
            Predicate.Contains(
              Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = true),
              Parameter(EndpointParameterName("value"))
            )
          ),
          Predicate.Equals(
            Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = true),
            Subtract(
              Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = false),
              Parameter(EndpointParameterName("value"))
            )
          ),
          Predicate.Equals(
            StatusCode, Literal(204)
          )
        )
      ) :: Nil
  )
}
