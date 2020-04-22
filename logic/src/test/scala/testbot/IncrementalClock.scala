package testbot

import cats.effect.Clock
import cats.effect.concurrent.Ref
import monix.eval.Task

import scala.concurrent.duration.TimeUnit

object IncrementalClock {
  implicit val instance: Clock[Task] = new Clock[Task] {
    import monix.execution.Scheduler.Implicits.global

    private val time = Ref.of[Task, Long](0).runSyncUnsafe()

    override def realTime(unit: TimeUnit): Task[Long] = time.modify(x => (x + 1, x))

    override def monotonic(unit: TimeUnit): Task[Long] = time.modify(x => (x + 1, x))
  }
}
