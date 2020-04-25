package autospec.demo

import cats.effect._
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import autospec.demo.RestApiSchema._
import autospec.runtime.applications.GeneratorConsoleApp

object RunGenerator extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    implicit val scheduler: Scheduler = Scheduler.traced

    Task.gather(
      List(
        new RestApi().run(),
        new GeneratorConsoleApp().run(schema)
      )
    ).as(ExitCode.Success).to[IO]
  }
}
