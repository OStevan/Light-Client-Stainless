package ch.epfl.ognjanovic.stevan.library.types

import stainless.lang._
import stainless.annotation._

object VotingPowers {
  def zero(): VotingPower = ZeroVotingPower
  def positive(power: BigInt): VotingPower = {
    require(power > 0)
    PositiveVotingPower(power)
  }

  sealed abstract class VotingPower {
    def power(): BigInt

    def +(other: VotingPower): VotingPower

    def *(multiplier: VotingPower): VotingPower

    def >(other: VotingPower): Boolean

    def >=(other: VotingPower): Boolean

    @law
    private def positive_power(): Boolean = {
      power() >= BigInt(0)
    }

    @law
    private def multiplier_law(other: VotingPower, temp: VotingPower): Boolean = {
      (this * other).power == (other * this).power &&
      (this.power * other.power) == (this * other).power &&
      (this * (other * temp)) == ((this * other) * temp)
    }

    @law
    private def addition_law(other: VotingPower, temp: VotingPower): Boolean = {
      (this + other).power == (other + this).power &&
      (this.power + other.power) == (this + other).power &&
      (this + (other + temp)) == ((this + other) + temp)
    }

    @law
    private def greater_law(other: VotingPower): Boolean = {
      this > other == this.power() > other.power()
    }

    @law
    private def greater_equal_law(other: VotingPower): Boolean = {
      this >= other == this.power() >= other.power()
    }
  }

  case object ZeroVotingPower extends VotingPower {
    def power(): BigInt = BigInt(0)

    override def +(other: VotingPower): VotingPower = other

    override def *(multiplier: VotingPower): VotingPower = ZeroVotingPower

    override def >(other: VotingPower): Boolean = false

    override def >=(other: VotingPower): Boolean = this > other || this == other
  }

  case class PositiveVotingPower(value: BigInt) extends VotingPower {
    require(value > BigInt(0))

    override def power(): BigInt = value

    override def +(other: VotingPower): VotingPower = other match {
      case PositiveVotingPower(power) => PositiveVotingPower(power + value)
      case ZeroVotingPower => this
    }

    override def *(multiplier: VotingPower): VotingPower = multiplier match {
      case PositiveVotingPower(value) => PositiveVotingPower(value * power)
      case ZeroVotingPower => ZeroVotingPower
    }

    override def >(other: VotingPower): Boolean = other match {
      case PositiveVotingPower(value) => power() > value
      case ZeroVotingPower => true
    }

    override def >=(other: VotingPower): Boolean = this == other || this > other
  }

  @library
  implicit class VotingPowerOptional(val votingPower: Option[PositiveVotingPower]) extends AnyVal {
    def power(): VotingPower = votingPower match {
      case Some(value) => value
      case None() => ZeroVotingPower
    }
  }
}
