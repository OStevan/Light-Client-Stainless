package ch.epfl.ognjanovic.stevan.library

case class Height(value: BigInt) {
  require(value >= BigInt(0))
}
