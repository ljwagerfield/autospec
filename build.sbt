name := "TestSpike"

version := "0.1"

scalaVersion := "2.13.1"

val silencerVersion = "1.4.4"

scalacOptions ++= Seq(
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
  "-Werror" // Fail the compilation if there are any warnings.
)

addCompilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full)


libraryDependencies ++= Seq(
  "org.typelevel"   %% "cats-core"                % "2.0.0",
  "org.typelevel"   %% "cats-effect"              % "2.0.0",
  "org.typelevel"   %% "alleycats-core"           % "2.0.0",
  "io.monix"        %% "monix"                    % "3.1.0",
  "org.http4s"      %% "http4s-dsl"               % "0.21.0-RC2",
  "org.http4s"      %% "http4s-async-http-client" % "0.21.0-RC2",
  "org.http4s"      %% "http4s-circe"             % "0.21.0-RC2",
  "io.circe"        %% "circe-core"               % "0.13.0-RC1",
  "io.circe"        %% "circe-generic"            % "0.13.0-RC1",
  "io.circe"        %% "circe-parser"             % "0.13.0-RC1",
  "com.github.ghik" %  "silencer-lib"             % silencerVersion % Provided cross CrossVersion.full,

  "org.scalatest"   %% "scalatest"                % "3.1.0" % Test,

  // Only used for the test app -- can be removed in future.
  "ch.qos.logback"  % "logback-classic"           % "1.2.3",
  "org.http4s"      %% "http4s-blaze-server"      % "0.21.0-RC2"
)