name := "Apart"

version := "1.0"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "src/junit")

javaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Test <<= baseDirectory(_ / "src/junit")

libraryDependencies += "junit" % "junit" % "4.11"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

fork in Test := true

// To run tests from the command line, uncomment and replace with the proper path form your system
//javaOptions in Test += "-Djava.library.path=/path-to-the-skelcl-folder/build/executor"