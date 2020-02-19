resolvers ++= Seq(
  Resolver.bintrayIvyRepo("epfl-lara", "sbt-plugins"),
  Resolver.bintrayRepo("epfl-lara", "princess"),
)

val StainlessVersion = "0.5.1"

addSbtPlugin("ch.epfl.lara" % "sbt-stainless" % StainlessVersion)

