package spike.runtime

import spike.BaseSymbols.Predicate
import spike.schema.ConditionId

case class EndpointRequestWithChecks(request: EndpointRequest, checks: Map[ConditionId, Predicate])