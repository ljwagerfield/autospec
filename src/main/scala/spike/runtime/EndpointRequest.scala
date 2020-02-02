package spike.runtime

import io.circe.Json
import spike.RuntimeSymbols._
import spike.schema.{EndpointId, EndpointParameterName}

case class EndpointRequest(id: EndpointId, parameterValues: scala.collection.Map[EndpointParameterName, Symbol]) {
  override def toString: String =
    s"${id.value}(${parameterValues.toList.map(x => s"${x._1} = '${x._2}'").mkString(", ")})"

  def resolveParameterValues(history: List[EndpointRequestResponse]): scala.collection.Map[EndpointParameterName, Json] =
    parameterValues.view.mapValues(SymbolResolver.resolveSymbol(history, _)).toMap
}
