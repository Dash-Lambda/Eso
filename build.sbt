ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "2.6.1"
ThisBuild / organization     := "com.github.dashlambda"
ThisBuild / organizationName := "dashlambda"

lazy val root = (project in file("."))
  .settings(
    name := "Eso",
    javaOptions += "-XX:+UnlockCommercialFeatures -Xmx8g", // Still not sure if this does anything
    Test / logBuffered := false,
    Test / parallelExecution := false,
    scalacOptions ++= Seq("-deprecation", "-feature"),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-fFW", "esoTestLog.txt", "-o"),
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases")),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "org.scala-lang" % "scala-compiler" % "2.13.3",
      "org.typelevel" %% "spire" % "0.17.0-M1",
      "org.typelevel" %% "jawn-parser" % "1.2.0",
      "org.typelevel" %% "jawn-ast" % "1.2.0",
      "org.scalactic" %% "scalactic" % "3.2.9",
      "org.scalatest" %% "scalatest" % "3.2.9" % "test"),
    assembly / assemblyJarName := s"Eso-${version.value}.jar",
    assembly / mainClass := Some("ui.NonPersistent"),
    assembly / assemblyOption ~= { _.withCacheOutput(false) }
  )