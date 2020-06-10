ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.12.9"

val circeVersion = "0.12.3"

lazy val circeDependencies = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

val lightClientCoreName = "light-client-core"
lazy val lightClientCore = project
  .in(file(lightClientCoreName))
  .enablePlugins(StainlessPlugin)
  .settings(
    name := lightClientCoreName,
    stainlessEnabled := false
)

lazy val tendermintGeneral = project
  .in(file("tendermint-general"))
  .enablePlugins(StainlessPlugin)
  .settings(
    name := "tendermint-general",
    stainlessEnabled := false,
    PB.targets in Compile := Seq(
      scalapb.gen(flatPackage = false) -> (sourceManaged in Compile).value
    ),
    Seq(
      libraryDependencies ++= circeDependencies ++ Seq(
        "org.scalamock" %% "scalamock" % "4.4.0" % Test,
        "org.scalactic" %% "scalactic" % "3.1.1",
        "org.scalatest" %% "scalatest" % "3.1.1" % Test,
        "io.grpc" % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion,
        "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
        "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
        "com.thesamet.scalapb" %% "scalapb-json4s" % scalapb.compiler.Version.scalapbVersion)))
  .dependsOn(lightClientCore)

val lightClientName = "light-client"
lazy val lightClient = project
  .in(file(lightClientName))
  .settings(
    name := lightClientName,
    Seq(
      libraryDependencies ++= circeDependencies ++ Seq(
        "org.scalamock" %% "scalamock" % "4.4.0" % Test,
        "org.scalactic" %% "scalactic" % "3.1.1",
        "org.scalatest" %% "scalatest" % "3.1.1" % Test,
        "com.google.crypto.tink" % "tink" % "1.4.0-rc2"
      ))
  )
  .dependsOn(tendermintGeneral)