name := "Apart"

version := "1.0"

scalaSource in Compile <<= baseDirectory(_ / "srci/main")

scalaSource in Test <<= baseDirectory(_ / "src/test")

javaSource in Compile <<= baseDirectory(_ / "src/main")

javaSource in Test <<= baseDirectory(_ / "src/test")

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
