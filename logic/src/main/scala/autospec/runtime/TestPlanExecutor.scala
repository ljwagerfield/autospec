package autospec.runtime

import alleycats.std.all._
import cats.implicits._
import monix.eval.Task

class TestPlanExecutor(stream: ValidationStreamFromTestPlan) {

  def executeMany(
    session: Session,
    plans: List[TestPlan],
    haltOnFailure: Boolean
  ): Task[Map[TestPlanId, List[ValidatedRequestResponseWithSymbols]]] =
    plans.map(x => x.id -> x).toMap.traverse(execute(session, _, haltOnFailure))

  def execute(
    session: Session,
    plan: TestPlan,
    haltOnFailure: Boolean
  ): Task[List[ValidatedRequestResponseWithSymbols]] =
    execute(session, plan.requests.toList, haltOnFailure)

  def execute(
    session: Session,
    plan: List[EndpointRequestSymbolic],
    haltOnFailure: Boolean
  ): Task[List[ValidatedRequestResponseWithSymbols]] = {
    val source   = stream(session, plan)
    val filtered = if (haltOnFailure) source.takeThrough(!_.isFailed) else source
    filtered.compile.toList
  }

}
