package spike.runtime

import io.circe.Json

/**
 * Payload of structured data (e.g. JSON, YAML, XML).
 *
 * We represent structured data in-memory using Circe JSON. However, the payload
 * we deserialize this from can be any structured data format (in theory).
 */
case class StructuredData(value: Json)
