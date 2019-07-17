import Dependencies._

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.github.dashlambda"
ThisBuild / organizationName := "dashlambda"

lazy val root = (project in file("."))
  .settings(
    name := "EsoInterpreter",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % "2.13.0",
      "org.typelevel" %% "spire" % "0.17.0-M1")
  )