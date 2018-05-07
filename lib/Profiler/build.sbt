lazy val root = (project in file("."))
  .settings(
    name          := "Profiler",
    organization  := "org.lift-project",
    version       := "1.0",
    scalaVersion  := "2.11.8",

    libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.8"
  )

val paradiseVersion = "2.1.0"
addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
