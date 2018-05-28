
lazy val root = (project in file("."))
  .settings(
    name          := "lift",
    organization  := "org.lift-project",
    version       := "1.0",
    scalaVersion  := "2.11.8",

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
    },

    // when compile also update submodules and compile the executor
    compile := ((compile in Compile) dependsOn (updateSubmodules, compileExecutor)).value,
    // Executor path
    javaOptions += "-Djava.library.path=" + baseDirectory(_ / "src/main/resources/lib/").value,

    scalacOptions ++= Seq("-Xmax-classfile-name", "100", "-unchecked", "-deprecation", "-feature"),
    scalacOptions in (Compile, doc) := Seq("-implicits", "-diagrams"),

    // Source locations (defaults would be: src/main/scala and test/main/java)
    scalaSource in Compile := baseDirectory(_ / "src/main").value,
    scalaSource in Test := baseDirectory(_ / "src/test").value,
    javaSource in Compile := baseDirectory(_ / "src/main").value,
    javaSource in Test := baseDirectory(_ / "src/test").value,

    // dependencies specified in project/Dependencies.scala
    libraryDependencies ++= Dependencies.libraryDependencies,

    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-a"),

    ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages := "<empty>;benchmarks.*;.*Test.*;junit.*;.*interop.*;.*arithmetic.*;.*testing.*",

    fork := true
  )
  .dependsOn(
    RootProject(file("lib/ArithExpr")),
    RootProject(file("lib/Profiler")))

// implicit dependency on the compiler plugin due to macros in profiler
val paradiseVersion = "2.1.0"
addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

lazy val updateSubmodules = taskKey[Unit]("Update the submodules")
updateSubmodules := {
  import scala.language.postfixOps
  import scala.sys.process._
  //noinspection PostfixMethodCall
  "echo y" #| "./updateSubmodules.sh" !
}

lazy val compileExecutor = taskKey[Unit]("Builds the Executor.")
compileExecutor := {
  import scala.language.postfixOps
  import scala.sys.process._
  //noinspection PostfixMethodCall
  "echo y" #| "./buildExecutor.sh" !
}

