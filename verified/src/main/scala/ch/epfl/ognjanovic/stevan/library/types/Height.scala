package ch.epfl.ognjanovic.stevan.library.types

case class Height(value: BigInt) {
  require(value >= BigInt(0))
}
