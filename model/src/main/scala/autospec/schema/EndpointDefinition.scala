package autospec.schema

import io.circe.Json
import autospec.LocalSchemaSymbols.Predicate.{Equals, Or}
import autospec.LocalSchemaSymbols.{Endpoint, Literal, Predicate, StatusCode}
import autospec.schema.SymbolExtensions._

case class EndpointDefinition(
  id: EndpointId,
  apiId: ApiId,
  method: HttpMethod,
  relativeUrl: String,
  parameters: List[EndpointParameter],
  preconditions: List[Precondition],
  postconditions: List[Predicate],
  forcePure: Boolean = false
) {

  val preconditionMap: Map[ConditionId, Precondition] =
    preconditions.zipWithIndex.map {
      case (precondition, index) =>
        ConditionId(id, ConditionType.Precondition, index) -> precondition
    }.toMap

  val postconditionMap: Map[ConditionId, Predicate] =
    postconditions.zipWithIndex.map {
      case (postcondition, index) =>
        ConditionId(id, ConditionType.Postcondition, index) -> postcondition
    }.toMap

  val parameterMap: Map[EndpointParameterName, EndpointParameter] =
    parameters.map(x => x.name -> x).toMap

  /**
    * Combines both preconditions and postconditions.
    */
  val conditions: Map[ConditionId, Predicate] = {
    val preconditionsAsPredicates =
      preconditionMap.view.mapValues {
        case Precondition(predicate, expectedStatus) =>
          Or(
            predicate,
            Equals(
              StatusCode,
              Literal(Json.fromInt(expectedStatus))
            )
          )
      }.toMap

    postconditionMap ++ preconditionsAsPredicates
  }

  lazy val isMutating: Boolean =
    !forcePure && postconditions
      .flatMap(_.toList)
      .collectFirst { case Endpoint(_, _, true) => () }
      .nonEmpty

  def parameter(name: EndpointParameterName): EndpointParameter =
    parameterMap.getOrElse(
      name,
      throw new Exception(
        s"Cannot find parameter '$name' for endpoint '${id.value}' in schema."
      )
    )

}
