package spike.runtime

import spike.RuntimeSymbols.Predicate
import spike.schema.ConditionId

case class EndpointRequestWithChecksOld(request: EndpointRequestSymbolic, checks: Map[ConditionId, Predicate])