package spike.runtime

import io.circe.Json

/**
 * @param status Response status code.
 * @param body Response body (if exists and if structured).
 *             Note: Although we represent the data in-memory using Circe JSON, the original payload
 *             could have been any structured serialization format (e.g. XML, YAML, etc.).
 */
case class EndpointResponse(status: Int, body: Json)