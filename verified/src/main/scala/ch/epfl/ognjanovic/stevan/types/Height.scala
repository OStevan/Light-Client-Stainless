package ch.epfl.ognjanovic.stevan.types

import stainless.annotation.opaque
import stainless.lang._

sealed case class Height(value: BigInt) {
  require(value > BigInt(0))

  def <=(other: Height): Boolean = value <= other.value

  def >=(other: Height): Boolean = !(this < other)

  def >(other: Height): Boolean = !(this <= other)

  def <(other: Height): Boolean = {
    this <= other && this != other
  }.ensuring(res => res == other > this)

  def +(value: BigInt): Height = {
    require(value > BigInt(0))
    Height(this.value + value)
  } ensuring (res => res.value == this.value + value)

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
