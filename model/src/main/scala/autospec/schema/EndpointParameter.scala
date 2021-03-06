package autospec.schema

case class EndpointParameter(
  name: EndpointParameterName,
  `type`: EndpointParameterType,
  location: EndpointParameterLocation,
  serialization: EndpointParameterSerialization
)
