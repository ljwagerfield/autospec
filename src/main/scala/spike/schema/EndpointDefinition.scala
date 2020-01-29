package spike.schema

import spike.SchemaSymbols.Predicate

case class EndpointDefinition(
  id: EndpointId,
  verb: String,
  url: String,
  parameters: List[EndpointParameter],
  preconditions: List[Precondition],
  postconditions: List[Predicate] // We also check the post-conditions on all referenced endpoints where 'evaluateAfterExecution==true'
)
