package testbot.runtime

import testbot.RuntimeSymbols.Predicate

case class EndpointRequestWithChecks(request: EndpointRequestSymbolic, checks: Set[Predicate])
