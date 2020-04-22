package autospec.schema

sealed trait EndpointParameterSerialization

object EndpointParameterSerialization {
  case class ToString(mediaType: String) extends EndpointParameterSerialization
  case object Json extends EndpointParameterSerialization
//  case object Yaml extends EndpointParameterFormat
//  case object Xml extends EndpointParameterFormat
}
