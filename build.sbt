// ---------------
// Versions
// ---------------

val appVersionString   = "0.1.0-SNAPSHOT"
val scalaVersionString = "2.13.1" // Docker base image must match.
val javaVersionString  = "11"      // Docker base image must match.
val silencerVersion    = "1.4.4"
val catsVersion        = "2.1.0"
val http4sVersion      = "0.21.0-RC2"
val circeVersion       = "0.13.0-RC1"

// ---------------
// Common Settings
// ---------------

val compileAndTest     = "compile->compile;test->test"

lazy val commonSettings = Seq(
  organization         := "io.projectone",
  version              := appVersionString,
  javacOptions        ++= Seq("-source", javaVersionString, "-target", javaVersionString),
  testOptions in Test  += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"),
  fork in run          := true,
  outputStrategy       := Some(StdoutOutput) // Send any forked output to stdout/err (instead of to SBT's logger, which can cause annoyances such as "[info] [DEBUG] Message").
)

lazy val commonScalaSettings = commonSettings ++: Seq(
  scalaVersion         := scalaVersionString,
  scalacOptions        := Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Wextra-implicit", // Warn when more than one implicit parameter section is defined.
    "-Wnumeric-widen", // Warn when numerics are widened.
    "-Wunused:implicits", // Warn if an implicit parameter is unused.
    "-Wunused:locals", // Warn if a local definition is unused.
    "-Wunused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Wunused:privates", // Warn if a private member is unused.
    "-Wunused:params", // Warn if a value parameter is unused.
    "-Wunused:imports", // Warn if unused imports
    "-Werror", // Fail the compilation if there are any warnings.
    s"-P:silencer:sourceRoots=${baseDirectory.value.getCanonicalPath}",
    "-P:silencer:pathFilters=target/scala-2\\.\\d+/(?:routes|twirl)/.*" // silence all warnings on autogenerated files
  ),
  resolvers += JCenterRepository,
  libraryDependencies ++= Seq(
    "org.scalatest"            %% "scalatest"    % "3.1.0"         % Test,
    "org.scalacheck"           %% "scalacheck"   % "1.14.3"        % Test,
    "org.mockito"               % "mockito-all"  % "1.10.19"       % Test,
    "com.softwaremill.macwire" %% "macros"       % "2.3.3"         % Provided,  // Provided because it's used at compile-time only.
    "com.github.ghik"          %% "silencer-lib" % silencerVersion % Provided cross CrossVersion.full,
    compilerPlugin("org.typelevel"   %% "kind-projector"  % "0.10.3"),
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion cross CrossVersion.full) // Allows silencing warning for generated files.
  )
)

// ---------------
// Projects
// ---------------

lazy val logic = (project in file("logic"))
  .dependsOn(macros)
  .dependsOn(model)
  .settings(commonScalaSettings: _*)
  .settings(
    name                 := "autospec-logic",
    libraryDependencies ++= Seq(
      "org.typelevel"   %% "cats-core"                % catsVersion,
      "org.typelevel"   %% "cats-effect"              % catsVersion,
      "org.typelevel"   %% "alleycats-core"           % catsVersion,
      "io.monix"        %% "monix"                    % "3.1.0",
      "org.slf4j"       %  "slf4j-api"                % "1.7.30",
      "org.http4s"      %% "http4s-dsl"               % http4sVersion,
      "org.http4s"      %% "http4s-async-http-client" % http4sVersion,
      "org.http4s"      %% "http4s-circe"             % http4sVersion,
      "io.circe"        %% "circe-core"               % circeVersion,
      "io.circe"        %% "circe-generic"            % circeVersion,
      "io.circe"        %% "circe-parser"             % circeVersion,
      "com.github.ghik" %  "silencer-lib"             % silencerVersion % Provided cross CrossVersion.full
    )
  )

lazy val macros = (project in file("macros"))
  .dependsOn(model)
  .settings(commonScalaSettings: _*)
  .settings(
    name                 := "autospec-macros"
  )

lazy val model = (project in file("model"))
  .settings(commonScalaSettings: _*)
  .settings(
    name                 := "autospec-model",
    libraryDependencies ++= Seq(
      "org.typelevel"   %% "cats-core"                % catsVersion,
      "org.typelevel"   %% "cats-effect"              % catsVersion,
      "io.circe"        %% "circe-core"               % circeVersion,
      "de.huxhorn.sulky" % "de.huxhorn.sulky.ulid"    % "8.2.0"
    )
  )

lazy val demo = (project in file("demo"))
  .dependsOn(logic)
  .settings(commonScalaSettings: _*)
  .settings(
    name                 := "autospec-demo",
    libraryDependencies ++= Seq(
      "ch.qos.logback"  % "logback-classic"           % "1.2.3",
      "org.http4s"      %% "http4s-blaze-server"      % "0.21.0-RC2"
    )
  )
