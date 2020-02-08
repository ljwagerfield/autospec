package spike.runtime

import spike.RuntimeSymbols.Predicate
import spike.schema.ConditionId

case class EndpointRequestWithChecks(request: EndpointRequest, checks: Map[ConditionId, Predicate])