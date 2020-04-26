package autospec.runtime

import autospec.schema.ConditionIdWithProvenance
import autospec.{RuntimeSymbolsExecuted => RE}

case class ValidatedRequestResponse(
  requestResponse: EndpointRequestResponse,
  resolvedConditions: Map[ConditionIdWithProvenance, (ConditionStatus, RE.Predicate)]
) {
  def request: EndpointRequest     = requestResponse.request
  def requestId: EndpointRequestId = requestResponse.requestId
  def response: EndpointResponse   = requestResponse.response
  def isFailed: Boolean            = resolvedConditions.values.exists(_._1.isFailed)
}
