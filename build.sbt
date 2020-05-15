ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.12.9"

lazy val tendermintRpc = project
  .in(file("tendermint-rpc"))
  .settings(
    name := "tendermint-rpc",
  )

lazy val lightClientCore = project
  .in(file("light-client-core"))
  .enablePlugins(StainlessPlugin)
  .settings(
    name := "light-client-core",
    stainlessEnabled := true
  )
