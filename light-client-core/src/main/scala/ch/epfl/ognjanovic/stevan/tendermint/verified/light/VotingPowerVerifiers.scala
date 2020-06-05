package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Commit, ValidatorSet, VotingPower}
import stainless.annotation.pure

object VotingPowerVerifiers {

  val defaultTrustVerifier: VotingPowerVerifier = ParameterizedVotingPowerVerifier(TrustLevel.default)

  abstract class VotingPowerVerifier {
    @pure
    def consensusObtained(validatorSet: ValidatorSet, commit: Commit): Boolean = {
      require(commit.forBlock subsetOf validatorSet.keys)
      ??? : Boolean
    }

    @pure
    def trustedCommit(validatorSet: ValidatorSet, commit: Commit): Boolean
  }

  case class ParameterizedVotingPowerVerifier(trustLevel: TrustLevel) extends VotingPowerVerifier {
    @pure
    override def consensusObtained(validatorSet: ValidatorSet, commit: Commit): Boolean = {
      validatorSet.nodesPower(commit.forBlock.toList) * VotingPower(3) > validatorSet.totalPower * VotingPower(2)
    }

    @pure
    override def trustedCommit(validatorSet: ValidatorSet, commit: Commit): Boolean = {
      trustLevel.denominator * validatorSet.nodesPower(commit.forBlock.toList).power() >
        validatorSet.totalPower.power() * trustLevel.numerator
    }
  }

}
