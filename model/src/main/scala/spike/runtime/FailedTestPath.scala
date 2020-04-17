package spike.runtime

import cats.data.NonEmptyList
import spike.schema.ConditionId

case class FailedTestPath(
  testPathId: TestPathId,
  failures: NonEmptyList[ConditionId],
  responses: NonEmptyList[EndpointResponse]
) {
  def lastRequestIndex: Int            = responses.size - 1
  def lastRequestId: EndpointRequestIdOld = EndpointRequestIdOld(testPathId, lastRequestIndex)
}