package spike.runtime

import spike.RuntimeSymbols.Symbol
import spike.schema.EndpointId

case class EndpointRequest(id: EndpointId, parameterValues: Map[String, Symbol]) {
  override def toString: String =
    s"${id.value}(${parameterValues.toList.map(x => s"${x._1} = '${x._2}'").mkString(", ")})"
}

