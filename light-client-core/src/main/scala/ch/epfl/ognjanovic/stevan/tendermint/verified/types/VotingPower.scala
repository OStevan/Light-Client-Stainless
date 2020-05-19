package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import stainless.annotation.invariant
import stainless.lang._

case class VotingPower(value: BigInt) {
  @invariant
  def invariant: Boolean = value >= 0

  def power(): BigInt = value

  def +(other: VotingPower): VotingPower = VotingPower(value + other.power())

  def *(other: VotingPower): VotingPower = VotingPower(value * other.value)

  def >(other: VotingPower): Boolean = value > other.power()

  def >=(other: VotingPower): Boolean = value >= other.power()

  def <=(other: VotingPower): Boolean = value <= other.value

  def isPositive: Boolean = value > BigInt(0)
}
