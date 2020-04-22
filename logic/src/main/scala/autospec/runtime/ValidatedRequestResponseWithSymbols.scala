package autospec.runtime

import autospec.runtime.ConditionStatus.ResolvedConditionStatus
import autospec.schema.ConditionIdWithProvenance

case class ValidatedRequestResponseWithSymbols(
    validatedRequestResponse: ValidatedRequestResponse,
    requestSymbolic: EndpointRequestSymbolic
  ) {
  def request: EndpointRequest     = validatedRequestResponse.request
  def requestId: EndpointRequestId = validatedRequestResponse.requestId
  def response: EndpointResponse   = validatedRequestResponse.response
  def isFailed: Boolean            = validatedRequestResponse.isFailed
  def resolvedConditions: Map[ConditionIdWithProvenance, ResolvedConditionStatus] =
    validatedRequestResponse.resolvedConditions
}
