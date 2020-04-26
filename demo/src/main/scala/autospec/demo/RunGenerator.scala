package autospec.demo

import autospec.demo.RestApiSchema._
import autospec.runtime.applications.GeneratorConsoleApp
import monix.eval.Task

object RunGenerator extends RunWithApi {
  override protected def runAutoSpec(): Task[Unit] = new GeneratorConsoleApp().run(schema)
}
