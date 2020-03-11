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

    def *(multiplier: BigInt): VotingPower = {
      require(multiplier >= BigInt(0))
      multiplier match {
        case a if a == BigInt(0) => ZeroVotingPower
        case _ => this match {
          case PositiveVotingPower(value) => PositiveVotingPower(multiplier * value)
          case ZeroVotingPower => ZeroVotingPower
        }
      }
    }

    def >(other: VotingPower): Boolean

    def >=(other: VotingPower): Boolean

    @law
    def positive_power(): Boolean = {
      power() >= BigInt(0)
    }
  }

  case object ZeroVotingPower extends VotingPower {
    def power(): BigInt = BigInt(0)

    override def +(other: VotingPower): VotingPower = other

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
