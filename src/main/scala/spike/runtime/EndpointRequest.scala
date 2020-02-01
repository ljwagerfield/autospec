package spike.runtime

import io.circe.Json
import spike.RuntimeSymbols._
import spike.schema.{EndpointId, EndpointParameterName}

case class EndpointRequest(id: EndpointId, parameterValues: scala.collection.Map[EndpointParameterName, Symbol]) {
  override def toString: String =
    s"${id.value}(${parameterValues.toList.map(x => s"${x._1} = '${x._2}'").mkString(", ")})"

  def resolveParameterValues(history: List[EndpointRequestResponse]): scala.collection.Map[EndpointParameterName, Json] =
    parameterValues.view.mapValues(resolveSymbol(history, _)).toMap

  private def resolveSymbol(history: List[EndpointRequestResponse], symbol: Symbol): Json =
    symbol match {
      case Literal(value)                   => value
      case Result(precedingRequestDistance) => history.drop(precedingRequestDistance).head.response.body
      case Map(symbol, path)                => path.foldLeft(resolveSymbol(history, symbol))(mapJson)
      case Flatten(symbol)                  => flatten(resolveSymbol(history, symbol))
    }

  private def mapJson(json: Json, key: String): Json =
    json.fold(
      Json.Null,
      _ => Json.Null,
      _ => Json.Null,
      _ => Json.Null,
      x => Json.JArray(x.map(mapJson(_, key))),
      x => x(key).getOrElse(Json.Null)
    )

  private def flatten(json: Json): Json =
    json
      .asArray
      .map(x => Json.JArray(x.flatMap(toVector)))
      .getOrElse(json)

  private def toVector(json: Json): Vector[Json] =
    json.asArray.getOrElse(Vector(json))
}
