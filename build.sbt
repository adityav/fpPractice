organization := "io.vish4"

name := "FPTry"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

ideaExcludeFolders += ".idea"

ideaExcludeFolders += ".idea_modules"

libraryDependencies ++= {
  Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.4",
  "org.specs2" %% "specs2" % "2.3.1" % "test"
  )
}