ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.12.9"

val circeVersion = "0.12.3"

lazy val circeDependencies = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

lazy val commonSettings: Seq[Setting[_]] = Seq(
  libraryDependencies ++= circeDependencies ++ Seq(
    "org.scalactic" %% "scalactic" % "3.1.1",
    "org.scalatest" %% "scalatest" % "3.1.1" % "test"
  )
)

lazy val tendermintRpc = project
  .in(file("tendermint-rpc"))
  .settings(
    name := "tendermint-rpc",
    commonSettings
  )

val lightClientCoreName = "light-client-core"
lazy val lightClientCore = project
  .in(file(lightClientCoreName))
  .enablePlugins(StainlessPlugin)
  .settings(
    name := lightClientCoreName,
    stainlessEnabled := true
)

val lightClientName = "light-client"
lazy val lightClient = project
  .in(file(lightClientName))
  .settings(
    name := lightClientName,
    commonSettings
  )
  .dependsOn(tendermintRpc)
