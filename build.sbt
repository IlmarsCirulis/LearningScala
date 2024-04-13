ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

lazy val root = (project in file("."))
  .settings(
    name := "LearningScala"
  )

libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % "3.2.18" % "test"
