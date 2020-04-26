package autospec.demo

import cats.effect._
import cats.implicits._
import monix.execution.Scheduler

object RunApi extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    implicit val scheduler: Scheduler = Scheduler.traced
    new RestApi().run().as(ExitCode.Success).to[IO]
  }

}
