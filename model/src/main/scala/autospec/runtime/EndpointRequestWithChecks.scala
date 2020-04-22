package autospec.runtime

import autospec.RuntimeSymbols.Predicate

case class EndpointRequestWithChecks(request: EndpointRequestSymbolic, checks: Set[Predicate])
