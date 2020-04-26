package autospec.demo

import cats.effect._
import fs2.concurrent.SignallingRef
import monix.eval.Task
import monix.execution.Scheduler

object RunApiOnly extends IOApp {
  implicit val scheduler: Scheduler = Scheduler.traced

  def run(args: List[String]): IO[ExitCode] = {
    for {
      exit <- SignallingRef[Task, Boolean](false)
      _    <- new RestApi().run(exit)
    } yield ExitCode.Success
  }.to[IO]

}
