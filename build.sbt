import ReleaseTransformations._

lazy val deboxSettings = Seq(
  organization := "org.spire-math",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  homepage := Some(url("http://github.com/non/debox")),

  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.10.6", "2.11.8"),

  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
  ),

  scalacOptions ++= Seq(
    "-Xlog-free-terms",
    "-feature",
    "-Yinline-warnings",
    "-deprecation",
    "-optimize",
    "-unchecked"
  ),

  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),

  publishTo <<= (version).apply { v =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  },

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
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges))

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val root = project
  .in(file("."))
  .dependsOn(core, spire, benchmark)
  .aggregate(core, spire, benchmark)
  .settings(moduleName := "debox")
  .settings(deboxSettings)
  .settings(noPublishSettings: _*)

lazy val core = project
  .in(file("core"))
  .settings(moduleName := "debox-core")
  .settings(deboxSettings)
  .settings(libraryDependencies += "org.spire-math" %% "spire-macros" % "0.11.0")

lazy val spire = project
  .in(file("spire"))
  .dependsOn(core)
  .settings(moduleName := "debox-spire")
  .settings(deboxSettings)
  .settings(libraryDependencies ++= Seq(
    "org.spire-math" %% "spire" % "0.11.0",
    "org.spire-math" %% "spire-laws" % "0.11.0" % "test",
    "org.typelevel" %% "discipline" % "0.6" % "test"))

lazy val benchmark = project
  .in(file("benchmark"))
  .dependsOn(core, spire)
  .settings(moduleName := "debox-benchmark")
  .settings(deboxSettings)
  .settings(Seq(
    javaOptions in run += "-Xmx3G",
    fork in run := true,
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "r09",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1")))
  .settings(noPublishSettings)
