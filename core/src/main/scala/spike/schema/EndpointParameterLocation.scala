package spike.schema

sealed trait EndpointParameterLocation

object EndpointParameterLocation {
  case object Body extends EndpointParameterLocation
  case object Path extends EndpointParameterLocation
  case object Querystring extends EndpointParameterLocation
  case object Header extends EndpointParameterLocation
}