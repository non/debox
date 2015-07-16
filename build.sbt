import sbt._
import sbt.Keys._

lazy val deboxSettings = Seq(
  organization := "org.spire-math",
  version := "0.7.2-SNAPSHOT",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.5", "2.11.7"),
  scalacOptions ++= Seq(
    "-Xlog-free-terms",
    "-feature",
    "-Yinline-warnings",
    "-deprecation",
    "-optimize",
    "-unchecked"
  ),
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    "org.spire-math" %% "spire" % "0.9.0",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
  ),
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
  })

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
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
  .settings(Seq(
    javaOptions in run += "-Xmx3G",
    fork in run := true,
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "r09",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1")))
  .settings(noPublishSettings)
