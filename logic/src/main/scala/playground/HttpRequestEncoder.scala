package playground

import io.circe.Json
import org.http4s
import org.http4s._
import org.http4s.headers.{`Content-Length`, `Content-Type`}
import org.http4s.implicits._
import testbot.runtime.EndpointRequest
import testbot.schema.HttpMethod._
import testbot.schema._

object HttpRequestEncoder {
  def encode[F[_]](schema: ApplicationSchema, request: EndpointRequest): Request[F] = {
    val endpoint   = schema.endpoint(request.endpointId)
    val api        = schema.api(endpoint.apiId)
    val method     = endpoint.method match {
      case Get    => Method.GET
      case Put    => Method.PUT
      case Post   => Method.POST
      case Delete => Method.DELETE
      case Patch  => Method.PATCH
    }
    val params =
      request
        .parameterValues
        .view
        .toList
        .map { case (name, json) => endpoint.parameter(name) -> json }
        .groupBy { case (parameter, _) => parameter.location }

    val serializedParams = params
      .get(_: EndpointParameterLocation)
      .toList
      .flatten
      .map { case (parameter, json) =>
        parameter.name.value -> serializeParameter(endpoint.id, parameter, json)
      }
      .toMap

    val (body, headersForBody) =
      params
        .get(EndpointParameterLocation.Body)
        .map(_.head)
        .map { case (parameter, json) =>
          val payload       = serializeParameter(endpoint.id, parameter, json)
          val contentType   = parameterContentType(parameter)
          val bytes         = payload.getBytes(contentType.charset.getOrElse(http4s.DefaultCharset).nioCharset)
          val contentLength = `Content-Length`.unsafeFromLong(bytes.length.toLong)
          val stream        = fs2.Stream.emits(bytes)
          stream -> List(contentType, contentLength)
        }
        .getOrElse(fs2.Stream.empty -> Nil)

    val headers     = serializedParams(EndpointParameterLocation.Header).toList.map(x => Header(x._1, x._2))
    val querystring = serializedParams(EndpointParameterLocation.Querystring)
    val path        = serializedParams(EndpointParameterLocation.Path).foldLeft(endpoint.relativeUrl) { (path, param) =>
      val (name, value) = param
      path.replaceAllLiterally(s":$name", value)
    }

    Request(
      method,
      Uri
        .unsafeFromString(s"${api.baseUrl}$path")
        .withQueryParams(querystring),
      HttpVersion.`HTTP/1.1`,
      Headers(headersForBody ::: headers),
      body
    )
  }

  private def serializeParameter(endpointId: EndpointId, parameter: EndpointParameter, value: Json): String =
    parameter.serialization match {
      case _: EndpointParameterSerialization.ToString =>
        value.fold(
          "",
          _.toString,
          _.toString,
          identity,
          _ => throw new Exception(s"Arrays are not supported for parameter '${parameter.name.value}' for endpoint '${endpointId.value}' because the parameter is serialized using 'toString'."),
          _ => throw new Exception(s"Complex objects are not supported for parameter '${parameter.name.value}' for endpoint '${endpointId.value}' because the parameter is serialized using 'toString'.")
        )
      case EndpointParameterSerialization.Json => value.noSpaces
    }

  private def parameterContentType(parameter: EndpointParameter): `Content-Type` =
    parameter.serialization match {
      case EndpointParameterSerialization.ToString(mediaType) =>
        `Content-Type`(MediaType.unsafeParse(mediaType), http4s.DefaultCharset)
      case EndpointParameterSerialization.Json =>
        `Content-Type`(mediaType"application/json", http4s.DefaultCharset)
    }
}

