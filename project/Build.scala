import sbt._
import sbt.Keys._

object MyBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
	  
	organization := "com.github.fommil",
    name := "debox",

    version := "0.3.0-SNAPSHOT",

    scalaVersion := "2.10.3",

    conflictWarning in ThisBuild := ConflictWarning.disable,

    scalacOptions ++= Seq(
      "-Xlog-free-terms",
      //"-Ymacro-debug-lite",
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
      "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
      "org.scala-lang" % "scala-reflect" % "2.10.3"
    ),
	licenses := Seq("MIT-style" -> url("http://opensource.org/licenses/mit-license.php")),
	homepage := Some(url("https://github.com/non/debox")),
	
	// http://www.scala-sbt.org/release/docs/Community/Using-Sonatype.html#sbt-sonatype-sbt
	// don't forget to create your ~/.sbt/0.13/sonatype.sbt and ~/.sbt/0.13/plugins/gpg.sbt
    publishMavenStyle := true,
	publishArtifact in Test := false,
	pomIncludeRepository := { _ => false },
    publishTo <<= version { v: String =>
         val nexus = "https://oss.sonatype.org/"
         if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
         else                             Some("releases" at nexus + "service/local/staging/deploy/maven2")
       },
   pomExtra := (
     <scm>
       <url>git@github.com:non/debox.git</url>
       <connection>scm:git:git@github.com:non/debox.git</connection>
     </scm>
     <developers>
       <developer>
         <id>non</id>
         <name>Erik Osheim</name>
       </developer>
     </developers>
    )
  )

  lazy val root = Project("debox", file("."))
  lazy val ext = Project("ext", file("ext")).dependsOn(root)
  lazy val benchmark:Project = Project("benchmark", file("benchmark")).settings(benchmarkSettings: _*).dependsOn(root, ext)

  val key = AttributeKey[Boolean]("javaOptionsPatched")

  def benchmarkSettings = Seq(
    // raise memory limits here if necessary
    javaOptions in run += "-Xmx6G",

    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "r09",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1"
    ),

    // enable forking in run
    fork in run := true,

    // custom kludge to get caliper to see the right classpath

    // we need to add the runtime classpath as a "-cp" argument to the
    // `javaOptions in run`, otherwise caliper will not see the right classpath
    // and die with a ConfigurationException. unfortunately `javaOptions` is a
    // SettingsKey and `fullClasspath in Runtime` is a TaskKey, so we need to
    // jump through these hoops here in order to feed the result of the latter
    // into the former
    onLoad in Global ~= { previous => state =>
      previous {
        state.get(key) match {
          case None =>
            // get the runtime classpath, turn into a colon-delimited string
            val classPath = Project.runTask(fullClasspath in Runtime in benchmark, state).get._2.toEither.right.get.files.mkString(":")
            // return a state with javaOptionsPatched = true and javaOptions set correctly
            Project.extract(state).append(Seq(javaOptions in (benchmark, run) ++= Seq("-cp", classPath)), state.put(key, true))

          case Some(_) => state // the javaOptions are already patched
        }
      }
    }

    // caliper stuff stolen shamelessly from scala-benchmarking-template
  )
}
