import sbt._

object Dependencies {
    lazy val libraryDependencies = Seq(
        // scala
        "org.scala-lang" % "scala-reflect" % "2.11.8",
        "org.scala-lang" % "scala-compiler" % "2.11.8",
        "org.scala-lang" % "scala-library" % "2.11.8",
    // testing
        "junit" % "junit" % "4.11",
        "com.novocode" % "junit-interface" % "0.11" % "test",
        "org.scalacheck" %% "scalacheck" % "1.13.0" % "test",
    // JSON & XML
        "com.typesafe.play" %% "play-json" % "2.3.10",
        "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.4",
    // cli parser
        "jline" % "jline" % "2.12.1",
        "commons-cli" % "commons-cli" % "1.3.1",
        "org.clapper" %% "argot" % "1.0.3",
    // Logging
        "ch.qos.logback" %  "logback-classic" % "1.1.7",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
    // Time utilities
        "com.github.nscala-time" %% "nscala-time" % "2.16.0"
    )
}