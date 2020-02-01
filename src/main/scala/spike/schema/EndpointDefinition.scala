package spike.schema

import spike.SchemaSymbols.Predicate

case class EndpointDefinition(
  id: EndpointId,
  apiId: ApiId,
  method: HttpMethod,
  relativeUrl: String,
  parameters: List[EndpointParameter],
  preconditions: List[Precondition],
  postconditions: List[Predicate] // We also check the post-conditions on all referenced endpoints where 'evaluateAfterExecution==true'
) {
  val parameterMap: Map[EndpointParameterName, EndpointParameter] = parameters.map(x => x.name -> x).toMap

  def parameter(name: EndpointParameterName): EndpointParameter =
    parameterMap.getOrElse(name, throw new Exception(s"Cannot find parameter '$name' for endpoint '${id.value}' in schema."))
}