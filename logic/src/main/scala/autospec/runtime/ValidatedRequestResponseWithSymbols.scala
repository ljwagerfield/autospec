package autospec.runtime

import autospec.schema.ConditionIdWithProvenance
import autospec.{RuntimeSymbolsExecuted => RE}

case class ValidatedRequestResponseWithSymbols(
  validatedRequestResponse: ValidatedRequestResponse,
  requestSymbolic: EndpointRequestSymbolic
) {
  def request: EndpointRequest     = validatedRequestResponse.request
  def requestId: EndpointRequestId = validatedRequestResponse.requestId
  def response: EndpointResponse   = validatedRequestResponse.response
  def isFailed: Boolean            = validatedRequestResponse.isFailed
  def resolvedConditions: Map[ConditionIdWithProvenance, (ConditionStatus, RE.Predicate)] =
    validatedRequestResponse.resolvedConditions
}
