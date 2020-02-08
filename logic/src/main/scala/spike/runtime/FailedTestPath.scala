package spike.runtime

import cats.data.NonEmptyList
import spike.schema.ConditionId

case class FailedTestPath(
  lastRequestId: EndpointRequestId,
  failures: NonEmptyList[ConditionId],
  responses: NonEmptyList[EndpointResponse]
) {
  def testPathId: TestPathId = lastRequestId.testPathId
  def lastRequestIndex: Int  = lastRequestId.requestIndex
}