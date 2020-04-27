package autospec.runtime

import alleycats.std.all._
import autospec.runtime.exceptions.HttpClientExceptionWithSymbols
import cats.implicits._
import fs2.Stream
import monix.eval.Task

class TestPlanExecutor(validatedStream: ValidatedStreamFromRequestStream) {

  def executeMany(
    session: Session,
    plans: List[TestPlan],
    haltOnFailure: Boolean
  ): Task[Map[TestPlanId, (Option[HttpClientExceptionWithSymbols], List[ValidatedRequestResponseWithSymbols])]] =
    plans.map(x => x.id -> x).toMap.traverse(execute(session, _, haltOnFailure))

  def execute(
    session: Session,
    plan: TestPlan,
    haltOnFailure: Boolean
  ): Task[(Option[HttpClientExceptionWithSymbols], List[ValidatedRequestResponseWithSymbols])] =
    execute(session, plan.requests.toList, haltOnFailure)

  def execute(
    session: Session,
    plan: List[EndpointRequestSymbolic],
    haltOnFailure: Boolean
  ): Task[(Option[HttpClientExceptionWithSymbols], List[ValidatedRequestResponseWithSymbols])] = {
    val requestStream = Stream.emits[Task, EndpointRequestSymbolic](plan)
    val source        = requestStream.through(validatedStream(session))
    val filtered      = if (haltOnFailure) source.takeThrough(_.forall(!_.isFailed)) else source
    val listF         = filtered.compile.toList
    listF.map(lastLeftAsOption)
  }

  private def lastLeftAsOption[A, B](value: List[Either[A, B]]): (Option[A], List[B]) =
    value.foldLeft((None: Option[A], List.empty[B])) { (accum, itemMaybe) =>
      val (opt, list) = accum
      itemMaybe.fold(a => Some(a) -> list, b => opt -> (list :+ b))
    }

}
