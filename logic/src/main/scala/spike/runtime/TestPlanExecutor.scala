package spike.runtime

import cats.data.{EitherT, NonEmptyList}
import monix.eval.Task
import cats.implicits._

class TestPlanExecutor(httpRequestExecutor: HttpRequestExecutor) {
  def execute(testPlan: TestPlan): Task[Map[EndpointRequestId, FailedTestPath]] =
    Task.wander(testPlan.paths) { path =>
      path.requests.zipWithIndex.foldM(List.empty[EndpointRequestResponseOld]) { (history, current) =>
        val (r, requestIndex) = current
        val httpRequest       = HttpRequestEncoderOld.encode[Task](testPlan.schema, history, r.request)
        val requestId         = EndpointRequestId(path.id, requestIndex)
        EitherT(
          httpRequestExecutor.execute(httpRequest).map { response =>
            ResponseValidator
              .validateResponse(history, r, response)
              .leftMap(conditions => requestId -> FailedTestPath(requestId, conditions, NonEmptyList(response, history.map(_.response)).reverse))
              .map(_ :: history)
          }
        )
      }.value.map(_.left.toOption)
    }.map(_.flatten.toMap)
}
