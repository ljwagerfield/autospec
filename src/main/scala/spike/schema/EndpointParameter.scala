package spike.schema

case class EndpointParameter(
  name: EndpointParameterName,
  location: EndpointParameterLocation,
  serialization: EndpointParameterSerialization
)
