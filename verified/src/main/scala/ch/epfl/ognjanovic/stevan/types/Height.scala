package ch.epfl.ognjanovic.stevan.types

case class Height(value: BigInt) {
  require(value >= BigInt(0))
}
