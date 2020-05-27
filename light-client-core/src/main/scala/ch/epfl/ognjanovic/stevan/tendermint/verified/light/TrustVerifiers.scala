package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Commit, ValidatorSet, VotingPower}
import stainless.annotation.pure

object TrustVerifiers {

  val defaultTrustVerifier: TrustVerifier = ParameterizedTrustVerifier(TrustLevel.default)

  abstract class TrustVerifier {
    @pure
    def consensusObtained(validatorSet: ValidatorSet, commit: Commit): Boolean = {
      require(commit.forBlock subsetOf validatorSet.keys)
      ??? : Boolean
    }

    @pure
    def trustedCommit(validatorSet: ValidatorSet, commit: Commit): Boolean
  }

  case class ParameterizedTrustVerifier(trustLevel: TrustLevel) extends TrustVerifier {
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
