package autospec.schema

sealed trait EndpointParameterType

object EndpointParameterType {
  case object Boolean extends EndpointParameterType
  case object String extends EndpointParameterType
  case object Int16 extends EndpointParameterType {
    val maxVal: Int = Math.pow(2, 15).toInt - 1
    val minVal: Int = Math.pow(-2, 15).toInt
  }
  case object Int32 extends EndpointParameterType
  case object Int64 extends EndpointParameterType
  case object Single extends EndpointParameterType
  case object Double extends EndpointParameterType
  case class Object(fields: Map[String, EndpointParameterType]) extends EndpointParameterType
  case class Array(elementTypes: List[EndpointParameterType]) extends EndpointParameterType {
    def isConstrained: Boolean = elementTypes.nonEmpty
  }
}
