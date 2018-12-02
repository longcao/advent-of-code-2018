lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2018",
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT")),
    libraryDependencies ++= List(
      Dependencies.fs2Core,
      Dependencies.fs2Io
    ))
