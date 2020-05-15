resolvers ++= Seq(
  Resolver.bintrayIvyRepo("epfl-lara", "sbt-plugins"),
  Resolver.bintrayRepo("epfl-lara", "smt-z3"),
  Resolver.mavenLocal
)

val StainlessVersion = "0.7.0-9-g67b2b7f"

addSbtPlugin("ch.epfl.lara" % "sbt-stainless" % StainlessVersion)

