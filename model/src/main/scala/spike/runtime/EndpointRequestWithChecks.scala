package spike.runtime

import spike.RuntimeSymbols.Predicate

case class EndpointRequestWithChecks(request: EndpointRequestSymbolic, checks: Set[Predicate])