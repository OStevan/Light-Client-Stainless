resolvers ++= Seq(
  Resolver.bintrayIvyRepo("epfl-lara", "sbt-plugins"),
  Resolver.bintrayRepo("epfl-lara", "smt-z3"),
  Resolver.mavenLocal
)

val StainlessVersion = "0.7.0-8-g85c0a70"

addSbtPlugin("ch.epfl.lara" % "sbt-stainless" % StainlessVersion)

