package ch.epfl.ognjanovic.stevan.types

import stainless.lang._

case class Height(value: BigInt) {
  require(value >= BigInt(0))
}
