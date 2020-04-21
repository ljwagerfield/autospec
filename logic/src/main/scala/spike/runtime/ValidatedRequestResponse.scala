package spike.runtime

import playground.EndpointRequestResponse
import spike.runtime.ConditionStatus.ResolvedConditionStatus
import spike.schema.ConditionIdWithProvenance

case class ValidatedRequestResponse(
  requestResponse: EndpointRequestResponse,
  resolvedConditions: Map[ConditionIdWithProvenance, ResolvedConditionStatus]
) {
  def request: EndpointRequest     = requestResponse.request
  def requestId: EndpointRequestId = requestResponse.requestId
  def response: EndpointResponse   = requestResponse.response
  def isFailed: Boolean            = resolvedConditions.values.exists(_.isFailed)
}