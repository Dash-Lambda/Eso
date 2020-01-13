ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.2.0"
ThisBuild / organization     := "com.github.dashlambda"
ThisBuild / organizationName := "dashlambda"

lazy val root = (project in file("."))
  .settings(
    name := "Eso",
    scalacOptions ++= Seq("-deprecation", "-feature"),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % "2.13.0",
      "org.typelevel" %% "spire" % "0.17.0-M1",
      "org.typelevel" %% "jawn-parser" % "0.14.2",
      "org.typelevel" %% "jawn-ast" % "0.14.2"))