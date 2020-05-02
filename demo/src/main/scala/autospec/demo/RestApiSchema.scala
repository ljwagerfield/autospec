package autospec.demo

import autospec.SchemaSymbols._
import autospec.schema._
import scala.collection.immutable.{Map => SMap}

object RestApiSchema {
  val apiId: ApiId = ApiId("api")

  val schema: ApplicationSchema = ApplicationSchema(
    ApiDefinition(apiId, "http://localhost:9005", addRequestIdHeader = true) :: Nil,
    EndpointDefinition(
      EndpointId("list"),
      apiId,
      HttpMethod.Get,
      "/foos",
      Nil,
      Nil,
      List(
        Predicate.Equals(
          Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = false),
          ResponseBody
        )
      )
    ) ::
      EndpointDefinition(
        EndpointId("get"),
        apiId,
        HttpMethod.Get,
        "/foos/:key",
        List(
          EndpointParameter(
            EndpointParameterName("key"),
            EndpointParameterType.String,
            EndpointParameterLocation.Path,
            EndpointParameterSerialization.ToString("text/plain")
          )
        ),
        List(
          Precondition(
            Predicate.Contains( // Todo: Create a ContainsKey to make this easier
              Map(
                Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = true),
                ValueAt(
                  LambdaParameter(0),
                  Literal("key")
                )
              ),
              Parameter("key")
            ),
            404
          )
        ),
        List(
          Predicate.Equals(
            StatusCode,
            Literal(200)
          )
        )
      ) ::
      EndpointDefinition(
        EndpointId("add"),
        apiId,
        HttpMethod.Post,
        "/foos/:key",
        List(
          EndpointParameter(
            EndpointParameterName("key"),
            EndpointParameterType.String,
            EndpointParameterLocation.Path,
            EndpointParameterSerialization.ToString("text/plain")
          ),
          EndpointParameter(
            EndpointParameterName("value"),
            EndpointParameterType.String,
            EndpointParameterLocation.Body,
            EndpointParameterSerialization.ToString("text/plain")
          )
        ),
        Nil,
        List(
          Predicate.Equals(
            ValueAt(
              Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = true),
              Parameter("key")
            ),
            Parameter("value")
          ),
          Predicate.Equals(
            StatusCode,
            Literal(200)
          )
        )
      ) ::
      EndpointDefinition(
        EndpointId("delete"),
        apiId,
        HttpMethod.Delete,
        "/foos/:key",
        List(
          EndpointParameter(
            EndpointParameterName("key"),
            EndpointParameterType.String,
            EndpointParameterLocation.Path,
            EndpointParameterSerialization.ToString("text/plain")
          )
        ),
        Nil,
        List(
          Predicate.Not(
            Predicate.Contains( // Todo: Create a ContainsKey to make this easier
              Map(
                Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = true),
                ValueAt(
                  LambdaParameter(0),
                  Literal("key")
                )
              ),
              Parameter("key")
            )
          ),
          Predicate.Equals(
            StatusCode,
            Literal(204)
          )
        )
      ) :: Nil
  )

}
