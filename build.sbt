name := "Apart"

version := "1.0"

scalaVersion := "2.11"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "junit")

javaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Test <<= baseDirectory(_ / "junit")

libraryDependencies += "junit" % "junit" % "4.11"
