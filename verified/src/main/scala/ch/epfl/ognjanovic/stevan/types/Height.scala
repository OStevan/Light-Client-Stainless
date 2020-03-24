package ch.epfl.ognjanovic.stevan.types

import stainless.lang._

case class Height(value: BigInt) {
  require(value > BigInt(0) && value <= BigInt(3))

  def +(value: BigInt): Height = {
    require(value > BigInt(0))
    Height(this.value + value)
  } ensuring(res => res.value == this.value + value)
}
