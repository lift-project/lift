name := "Lift"

version := "1.0"

scalaVersion := "2.11.8"

// Check Java version
initialize := {
  val _ = initialize.value // run the previous initialization
  val minVersion = 8
  val current  = sys.props("java.specification.version")
  val regex = raw"1\.(\d+)".r
  assert(current match {
    case regex(v) if v.toInt >= minVersion => true
    case _ => false
  }, s"Unsupported JDK: java.specification.version $current. Require at least JDK version 1.$minVersion.")
}

compile <<= (compile in Compile) dependsOn (updateSubmodules/*, compileExecutor*/)

lazy val updateSubmodules = taskKey[Unit]("Update the submodules")

updateSubmodules := {
  import scala.language.postfixOps
  import scala.sys.process._
  //noinspection PostfixMethodCall
  "echo y" #| "./updateSubmodules.sh" !
}

//lazy val compileExecutor = taskKey[Unit]("Builds the Executor.")
//
//compileExecutor := {
//  import scala.language.postfixOps
//  import scala.sys.process._
//  //noinspection PostfixMethodCall
//  "echo y" #| "./buildExecutor.sh" !
//}

scalacOptions ++= Seq("-Xmax-classfile-name", "100", "-unchecked", "-deprecation", "-feature")

javaOptions ++= Seq(
  "-Djava.library.path=" + baseDirectory(_ / "src/main/resources/lib/").value,
  "-Xss4m"
)

// Main sources
scalaSource in Compile <<= baseDirectory(_ / "src/main")
javaSource in Compile <<= baseDirectory(_ / "src/main")

// Test sources
scalaSource in Test <<= baseDirectory(_ / "src/test")
javaSource in Test <<= baseDirectory(_ / "src/test")

// Scala libraries
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.8"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.8"
libraryDependencies += "org.scala-lang" % "scala-library" % "2.11.8"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.7.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.4"

libraryDependencies += "jline" % "jline" % "2.12.1"

// JUnit
libraryDependencies += "junit" % "junit" % "4.11"
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

// ScalaCheck
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

// TODO: Pick one for argument parsing
libraryDependencies += "commons-cli" % "commons-cli" % "1.4"
// Argot is abandoned and does not support Scala 2.12.0
//libraryDependencies += "org.clapper" %% "argot" % "1.0.3"
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"

// Logging
libraryDependencies += "ch.qos.logback" %  "logback-classic" % "1.1.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

// https://mvnrepository.com/artifact/org.jfree/jfreesvg
libraryDependencies += "org.jfree" % "jfreesvg" % "2.0"

// Time utilities
libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.16.0"
// Constraint solver dependencies
libraryDependencies += "org.choco-solver" % "choco-solver" % "4.10.1"

lazy val profiler = RootProject(file("lib/Profiler"))

lazy val root = (project in file(".")).aggregate(profiler).dependsOn(profiler)

val paradiseVersion = "2.1.0"

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)


scalacOptions in (Compile,doc) := Seq("-implicits", "-diagrams")

// Build ArithExpr
unmanagedSourceDirectories in Compile += baseDirectory.value / "lib/ArithExpr/src/main/"

scoverage.ScoverageKeys.coverageExcludedPackages := "<empty>;benchmarks.*;.*Test.*;junit.*;.*interop.*;.*arithmetic.*;.*testing.*"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-a")

fork := true
