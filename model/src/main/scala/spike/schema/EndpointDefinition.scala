package spike.schema

import spike.SchemaSymbols.Predicate

case class EndpointDefinition(
  id: EndpointId,
  apiId: ApiId,
  method: HttpMethod,
  relativeUrl: String,
  parameters: List[EndpointParameter],
  preconditions: List[Precondition],
  postconditions: List[Predicate], // We also check the post-conditions on all referenced endpoints where 'evaluateAfterExecution==true'
  forcePure: Boolean = false
) {
  val preconditionMap: Map[ConditionId, Precondition] =
    preconditions.zipWithIndex.map { case (precondition, index) =>
      ConditionId(id, ConditionType.Precondition, index) -> precondition
    }.toMap

  val postconditionMap: Map[ConditionId, Predicate] =
    postconditions.zipWithIndex.map { case (postcondition, index) =>
      ConditionId(id, ConditionType.Postcondition, index) -> postcondition
    }.toMap

  val parameterMap: Map[EndpointParameterName, EndpointParameter] = parameters.map(x => x.name -> x).toMap

  def parameter(name: EndpointParameterName): EndpointParameter =
    parameterMap.getOrElse(name, throw new Exception(s"Cannot find parameter '$name' for endpoint '${id.value}' in schema."))
}