import ReleaseTransformations._

lazy val scalac: Seq[String] = Seq(
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
  "-encoding", "utf-8",                // Specify character encoding used by source files.
  "-explaintypes",                     // Explain type errors in more detail.
  "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
  "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
  "-language:higherKinds",             // Allow higher-kinded types
  "-language:implicitConversions",     // Allow definition of implicit functions called views
  "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
  "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
  // "-Ypartial-unification",             // Enable partial unification in type constructor inference
  "-Ywarn-dead-code",                  // Warn when dead code is identified.
  "-Ywarn-numeric-widen",              // Warn when numerics are widened.
  //"-Xlog-free-terms",
  "-feature",
  "-deprecation",
  "-unchecked"
)

lazy val scalac211: Seq[String] = Seq(
  "-optimize",
  "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
  "-Xlint:unsound-match",              // Pattern match may not be typesafe.
  "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
  "-Xfuture",                          // Turn on future language features.
)

lazy val scalac212: Seq[String] = Seq(
  "-opt:simplify-jumps",
  "-opt:compact-locals",
  "-opt:copy-propagation",
  "-opt:box-unbox",
  "-opt:closure-invocations",
  "-opt:unreachable-code",
  "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
  "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",              // Warn if a local definition is unused.
  "-Ywarn-unused:params",              // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",            // Warn if a private member is unused.
  "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
  "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
  "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
  "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",            // Option.apply used implicit view.
  "-Xlint:package-object-classes",     // Class or object defined in package object.
  "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match",              // Pattern match may not be typesafe.
  "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Xfuture",                          // Turn on future language features.
)

lazy val scalac213: Seq[String] = Seq(
  "-opt:simplify-jumps",
  "-opt:compact-locals",
  "-opt:copy-propagation",
  "-opt:box-unbox",
  "-opt:closure-invocations",
  "-opt:unreachable-code",
  "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",              // Warn if a local definition is unused.
  "-Ywarn-unused:params",              // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",            // Warn if a private member is unused.
  "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
  "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
  "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
  "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
  "-Xlint:option-implicit",            // Option.apply used implicit view.
  "-Xlint:package-object-classes",     // Class or object defined in package object.
  "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
)

lazy val deboxSettings = Seq(
  organization := "org.spire-math",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  homepage := Some(url("http://github.com/non/debox")),

  scalaVersion := "2.13.6",
  crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.6"),

  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "spire" % "0.17.0-M1",
    "org.scalatest" %% "scalatest" % "3.3.0-SNAP3" % Test,
    "org.scalatest" %% "scalatest-propspec" % "3.3.0-SNAP3" % Test,
    "org.scalatest" %% "scalatest-shouldmatchers" % "3.3.0-SNAP3" % Test,
    "org.scalatestplus" %% "scalacheck-1-15" % "3.3.0.0-SNAP3" % Test,
    "org.scalacheck" %% "scalacheck" % "1.15.2" % Test
  ),


  scalacOptions := {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n == 13 =>
        scalac ++ scalac213
      case Some((2, n)) if n == 12 =>
        scalac ++ scalac212
      case Some((2, 11)) =>
        scalac ++ scalac211
    }
  },

  libraryDependencies := {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, n)) if n >= 11 =>
        libraryDependencies.value
      // in Scala 2.10, quasiquotes are provided by macro-paradise
      case Some((2, 10)) =>
        libraryDependencies.value ++ Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
          "org.scalamacros" %% "quasiquotes" % "2.0.1")
    }
  },

  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := Function.const(false),

  publishTo := Some(if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging),

  pomExtra := (
    <scm>
      <url>git@github.com:non/debox.git</url>
      <connection>scm:git:git@github.com:non/debox.git</connection>
    </scm>
    <developers>
      <developer>
        <id>d_m</id>
        <name>Erik Osheim</name>
        <url>http://github.com/non/</url>
      </developer>
    </developers>
  ),

  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    pushChanges))

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

lazy val core = project
  .in(file("."))
  .settings(moduleName := "debox")
  .settings(deboxSettings)

lazy val benchmark = project.dependsOn(core)
  .in(file("benchmark"))
  .settings(moduleName := "debox-benchmark")
  .settings(deboxSettings)
  .enablePlugins(JmhPlugin)
  .settings(Seq(
    run / javaOptions += "-Xmx3G",
    run / fork := true))
  .settings(noPublishSettings)
