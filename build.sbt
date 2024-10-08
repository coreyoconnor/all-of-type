val scala3Version = "3.5.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "all-of-type",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions += "-experimental",

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
