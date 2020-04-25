package autospec.runtime

import autospec.RuntimeSymbolsIndexed.Predicate

case class EndpointRequestWithChecks(request: EndpointRequestSymbolic, checks: Set[Predicate])
