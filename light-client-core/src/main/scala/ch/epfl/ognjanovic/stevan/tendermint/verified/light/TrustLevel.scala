package ch.epfl.ognjanovic.stevan.tendermint.verified.light

case class TrustLevel(numerator: BigInt, denominator: BigInt) {
  require(3 * numerator >= denominator)
}

object TrustLevel {
  val default: TrustLevel = TrustLevel(1, 3)
}
