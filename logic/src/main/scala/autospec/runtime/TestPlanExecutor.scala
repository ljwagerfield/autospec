package autospec.runtime

import alleycats.std.all._
import autospec.runtime.exceptions.EndpointRequestFailureWithSymbols
import cats.implicits._
import fs2.Stream
import monix.eval.Task

class TestPlanExecutor(validatedStream: ValidatedStreamFromRequestStream) {

  def executeMany(
    session: Session,
    plans: List[TestPlan],
    haltOnFailure: Boolean
  ): Task[Map[TestPlanId, List[Either[EndpointRequestFailureWithSymbols, ValidatedRequestResponseWithSymbols]]]] =
    plans.map(x => x.id -> x).toMap.traverse(execute(session, _, haltOnFailure))

  def execute(
    session: Session,
    plan: TestPlan,
    haltOnFailure: Boolean
  ): Task[List[Either[EndpointRequestFailureWithSymbols, ValidatedRequestResponseWithSymbols]]] =
    execute(session, plan.requests.toList, haltOnFailure)

  def execute(
    session: Session,
    plan: List[EndpointRequestSymbolic],
    haltOnFailure: Boolean
  ): Task[List[Either[EndpointRequestFailureWithSymbols, ValidatedRequestResponseWithSymbols]]] = {
    val requestStream = Stream.emits[Task, EndpointRequestSymbolic](plan)
    val source        = requestStream.through(validatedStream(session))
    val filtered      = if (haltOnFailure) source.takeThrough(_.exists(!_.isFailed)) else source
    filtered.compile.toList
  }

}
