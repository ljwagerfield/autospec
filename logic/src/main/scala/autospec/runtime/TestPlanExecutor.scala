package autospec.runtime

import alleycats.std.all._
import cats.implicits._
import monix.eval.Task
import playground.ValidationStreamFromTestPlan
import autospec.schema.ApplicationSchema

class TestPlanExecutor(stream: ValidationStreamFromTestPlan) {

  def executeMany(
    schema: ApplicationSchema,
    plans: List[TestPlan],
    haltOnFailure: Boolean
  ): Task[Map[TestPlanId, List[ValidatedRequestResponseWithSymbols]]] =
    plans.map(x => x.id -> x).toMap.traverse(execute(schema, _, haltOnFailure))

  def execute(
    schema: ApplicationSchema,
    plan: TestPlan,
    haltOnFailure: Boolean
  ): Task[List[ValidatedRequestResponseWithSymbols]] =
    execute(schema, plan.requests.toList, haltOnFailure)

  def execute(
    schema: ApplicationSchema,
    plan: List[EndpointRequestSymbolic],
    haltOnFailure: Boolean
  ): Task[List[ValidatedRequestResponseWithSymbols]] = {
    val source   = stream(schema, plan)
    val filtered = if (haltOnFailure) source.takeThrough(!_.isFailed) else source
    filtered.compile.toList
  }
}
