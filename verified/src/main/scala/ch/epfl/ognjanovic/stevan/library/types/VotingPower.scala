package ch.epfl.ognjanovic.stevan.library.types

import stainless.lang._
import stainless.annotation._

object VotingPower {
    sealed abstract class VotingPower {
      def power(): BigInt

      def +(other: VotingPower): VotingPower

      @law
      def positive_power(): Boolean = {
          power() >= BigInt(0)
      }
  }

  case object ZeroVotingPower extends VotingPower {
      def power(): BigInt = BigInt(0)
      def +(other: VotingPower): VotingPower = other
  }

  case class PositiveVotingPower(value: BigInt) extends VotingPower {
      require(value > BigInt(0))

      def power(): BigInt = value

      def +(other: VotingPower): VotingPower = other match {
          case PositiveVotingPower(power) => PositiveVotingPower(power + value)
          case ZeroVotingPower => this
      }
  }

  @library
  implicit class VotingPowerOptional(val votingPower: Option[PositiveVotingPower]) extends AnyVal {
      def power(): VotingPower = votingPower match {
          case Some(value) => value
          case None() => ZeroVotingPower
      }
  }
}
