package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import stainless.annotation.{invariant, opaque}
import stainless.lang._

case class Height(value: BigInt) {

  @invariant
  def invariant: Boolean = value > 0

  def <=(other: Height): Boolean = value <= other.value

  def >=(other: Height): Boolean = value >= other.value

  def >(other: Height): Boolean = value > other.value

  def <(other: Height): Boolean = value < other.value

  def +(value: BigInt): Height = {
    require(value > BigInt(0))
    Height(this.value + value)
  }

  def +(other: Height): Height = {
    Height(this.value + other.value)
  }

  def /(value: BigInt): Height = {
    require(value > BigInt(0) && this.value > BigInt(1))
    Height(this.value / 2)
  }
}

object Height {
  implicit def min(first: Height, second: Height): Height = {
    if (first.value < second.value)
      first
    else
      second
  } ensuring (res => (res == first || res == second) && (res.value <= first.value && res.value <= second.value))

  @opaque
  def helperLemma(first: Height, second: Height, third: Height): Unit = {
    require(first <= second && second < third)
  }.ensuring(_ => first < third)
}
