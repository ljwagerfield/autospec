package autospec.demo

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.concurrent.SignallingRef
import monix.eval.Task
import monix.execution.Scheduler

abstract class RunWithApi extends IOApp {
  implicit protected val scheduler: Scheduler = Scheduler.traced

  def run(args: List[String]): IO[ExitCode] = {
    implicit val scheduler: Scheduler = Scheduler.traced

    for {
      exit <- SignallingRef[Task, Boolean](false)
      _ <- Task.gather(
             List(
               new RestApi().run(exit)
             ).filterNot(_ => args.contains_("--no-dev-server")) :::
               List(
                 runAutoSpec().flatMap(_ => exit.set(true))
               )
           )
    } yield ExitCode.Success
  }.to[IO]

  protected def runAutoSpec(): Task[Unit]
}
