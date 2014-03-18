import sbt._
import sbt.Keys._

object MyBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    name := "debox",
    organization := "org.spire-math",
    version := "0.5.0-SNAPSHOT",

    scalaVersion := "2.10.3",

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

    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full),

    libraryDependencies ++= Seq(
      "org.spire-math" %% "spire" % "0.7.3",
      "org.scala-lang" % "scala-reflect" % "2.10.3" % "provided",
      "org.scalamacros" % "quasiquotes_2.10.3" % "2.0.0-M3" % "provided",
      "org.scalatest" %% "scalatest" % "2.0" % "test",
      "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
    )
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
