package ch.epfl.ognjanovic.stevan.tendermint.verified.types

/**
 * Time duration used for expiration checks.
 */
case class Duration(seconds: BigInt, nanos: BigInt) {
  require(seconds >= 0 && nanos >= 0)
}
