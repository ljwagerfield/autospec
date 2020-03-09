package spike.runtime

import spike.RuntimeSymbols.Predicate
import spike.schema.ConditionId

case class EndpointRequestWithChecks(request: EndpointRequestOld, checks: Map[ConditionId, Predicate])