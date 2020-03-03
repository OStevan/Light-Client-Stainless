resolvers ++= Seq(
  Resolver.bintrayIvyRepo("epfl-lara", "sbt-plugins"),
  Resolver.bintrayRepo("epfl-lara", "smt-z3"),
)

val StainlessVersion = "0.7.0"

addSbtPlugin("ch.epfl.lara" % "sbt-stainless" % StainlessVersion)

