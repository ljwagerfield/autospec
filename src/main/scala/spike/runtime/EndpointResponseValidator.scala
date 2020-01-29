package spike.runtime

import spike.schema.ApplicationSchema

class EndpointResponseValidator {
  def apply(schema: ApplicationSchema)(history: List[EndpointRequestResponse], current: EndpointRequestResponse): List[ConditionViolation] = {

  }
}
