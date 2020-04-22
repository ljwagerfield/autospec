package testbot.runtime

import io.circe.Json

/**
 * @param status Response status code.
 * @param body Response body (if exists and if structured).
 *             Note: Although we represent the data in-memory using Circe JSON, the original payload
 *             could have been any structured serialization format (e.g. XML, YAML, etc.).
 */
case class EndpointResponse(status: Int, body: Json, bodyRaw: String) {
  override def toString: String =
    s"($status) ${if (body.isNull) truncate(bodyRaw.replaceAllLiterally("\n", "\\n")) else body.noSpaces}"

  private def truncate(value: String): String =
    if (value.length > 150)
      s"'${value.substring(0, 150)}...'"
    else if (value.isEmpty)
      "<no body>"
    else
      s"'$value'"
}
