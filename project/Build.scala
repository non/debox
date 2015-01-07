import sbt._
import sbt.Keys._

object MyBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    name := "debox",
    organization := "org.spire-math",
    version := "0.7.0",

    scalaVersion := "2.11.4",

    crossScalaVersions := Seq("2.10.4", "2.11.4"),

    conflictWarning in ThisBuild := ConflictWarning.disable,

    scalacOptions ++= Seq(
      "-Xlog-free-terms",
      "-feature",
      "-Yinline-warnings",
      "-deprecation",
      "-optimize",
      "-unchecked"
    ),

    resolvers += Resolver.sonatypeRepo("releases"),

    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      "org.spire-math" %% "spire" % "0.9.0",
      "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
    ),

    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          libraryDependencies.value

        // in Scala 2.10, quasiquotes are provided by macro-paradise
        case Some((2, 10)) =>
          libraryDependencies.value ++ Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
            "org.scalamacros" %% "quasiquotes" % "2.0.1")
      }
    }
  )

  lazy val root = Project("debox", file("."))

  lazy val benchmark = Project("benchmark", file("benchmark")).settings(

    // raise memory limits here if necessary
    javaOptions in run += "-Xmx3G",

    // enable forking in run
    fork in run := true,

    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "r09",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1")

  ).dependsOn(root)
}
