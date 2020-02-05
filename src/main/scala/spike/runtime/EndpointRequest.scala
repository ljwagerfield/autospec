package spike.runtime

import cats.kernel.Eq
import io.circe.Json
import spike.RuntimeSymbols._
import spike.schema.{EndpointId, EndpointParameterName}

case class EndpointRequest(endpointId: EndpointId, parameterValues: scala.collection.immutable.Map[EndpointParameterName, Symbol]) {
  override def toString: String =
    s"${endpointId.value}(${parameterValues.toList.map(x => s"${x._1.value} = ${x._2}").mkString(", ")})"

  def resolveParameterValues(history: List[EndpointRequestResponse]): scala.collection.Map[EndpointParameterName, Json] =
    parameterValues.view.mapValues(SymbolResolver.resolveSymbol(history, _)).toMap

  def parameterValue(name: EndpointParameterName): Symbol =
    parameterValues.getOrElse(name, throw new Exception(s"Cannot find value for parameter '${name.value}' for endpoint '${endpointId.value}' in test path."))
}

object EndpointRequest {
  implicit val eq: Eq[EndpointRequest] = Eq.fromUniversalEquals
}