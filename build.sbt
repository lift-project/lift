name := "Apart"

version := "1.0"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "src/junit")

javaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Test <<= baseDirectory(_ / "src/junit")

libraryDependencies += "junit" % "junit" % "4.11"

libraryDependencies += "com.twitter" %% "util-eval" % "6.12.1"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "org.clapper" %% "argot" % "1.0.3"

scalacOptions in (Compile,doc) := Seq("-implicits", "-diagrams")

ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages := "<empty>;benchmarks.*;.*Test.*;junit.*;.*interop.*"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

fork := true

// To run tests from the command line, uncomment and replace with the proper path form your system
//javaOptions in Test += "-Djava.library.path=/path-to-the-skelcl-folder/build/executor"
