name := "hashes"

organization := "com.arturmkrtchyan"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.5"


libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "2.4.15" % "test",
  "com.google.guava" % "guava" % "17.0" % "compile"
)
