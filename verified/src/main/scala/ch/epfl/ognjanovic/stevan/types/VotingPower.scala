package ch.epfl.ognjanovic.stevan.types

import stainless.lang._

sealed case class VotingPower(value: BigInt) {
  require(value >= BigInt(0))

  def power(): BigInt = value

  def +(other: VotingPower): VotingPower = VotingPower(value + other.power())

  def *(other: VotingPower): VotingPower = VotingPower(value * other.value)

  def >(other: VotingPower): Boolean = value > other.power()

  def >=(other: VotingPower): Boolean = value >= other.power()

  def <=(other: VotingPower): Boolean = !(this > other)

  def isPositive: Boolean = value > BigInt(0)
}
