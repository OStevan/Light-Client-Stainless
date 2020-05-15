ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.12.9"

lazy val core = project
  .in(file("core"))
  .settings(
    name := "test",
  )
  .dependsOn(lightClientCore)

lazy val lightClientCore = project
  .in(file("light-client-core"))
  .enablePlugins(StainlessPlugin)
  .settings(
    name := "light-client-core",
    stainlessEnabled := true
  )
