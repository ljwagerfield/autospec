package spike.runtime

import cats.data.{EitherT, NonEmptyList}
import monix.eval.Task
import cats.implicits._
import spike.runtime.http.{HttpRequestEncoder, HttpRequestExecutor}
import spike.schema.ConditionId

class TestPlanExecutor(httpRequestEncoder: HttpRequestEncoder, httpRequestExecutor: HttpRequestExecutor) {
  def apply(testPlan: TestPlan): Task[Map[EndpointRequestId, NonEmptyList[ConditionId]]] =
    Task.wander(testPlan.paths) { path =>
      path.requests.zipWithIndex.foldM(List.empty[EndpointRequestResponse]) { (history, current) =>
        val (r, requestIndex) = current
        val httpRequest       = httpRequestEncoder[Task](testPlan.schema, history, r.request)
        val requestId         = EndpointRequestId(path.id, requestIndex)
        EitherT(
          httpRequestExecutor(httpRequest).map { response =>
            r.validateResponse(history, response)
              .leftMap(requestId -> _)
              .map(_ :: history)
          }
        )
      }.value.map(_.left.toOption)
    }.map(_.flatten.toMap)
}
