name := "Profiler"

version := "1.0"

scalaVersion := "2.12.0"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.12.0"
)

val paradiseVersion = "2.1.0"

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
