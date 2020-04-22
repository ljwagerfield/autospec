package testbot.runtime

import playground.EndpointRequestResponse
import testbot.runtime.ConditionStatus.ResolvedConditionStatus
import testbot.schema.ConditionIdWithProvenance

case class ValidatedRequestResponse(
  requestResponse: EndpointRequestResponse,
  resolvedConditions: Map[ConditionIdWithProvenance, ResolvedConditionStatus]
) {
  def request: EndpointRequest     = requestResponse.request
  def requestId: EndpointRequestId = requestResponse.requestId
  def response: EndpointResponse   = requestResponse.response
  def isFailed: Boolean            = resolvedConditions.values.exists(_.isFailed)
}
