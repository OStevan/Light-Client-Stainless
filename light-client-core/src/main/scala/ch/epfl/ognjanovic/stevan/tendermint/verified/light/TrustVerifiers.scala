package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Commit, ValidatorSet, VotingPower}

object TrustVerifiers {

  val defaultTrustVerifier: TrustVerifier = ParameterizedTrustVerifier(TrustLevel.default)

  abstract class TrustVerifier {
    def consensusObtained(validatorSet: ValidatorSet, commit: Commit): Boolean

    def trustedCommit(validatorSet: ValidatorSet, commit: Commit): Boolean
  }

  case class ParameterizedTrustVerifier(trustLevel: TrustLevel) extends TrustVerifier {
    override def consensusObtained(validatorSet: ValidatorSet, commit: Commit): Boolean = {
      validatorSet.nodesPower(commit.forBlock.toList) * VotingPower(3) > validatorSet.totalPower * VotingPower(2)
    }

    override def trustedCommit(validatorSet: ValidatorSet, commit: Commit): Boolean = {
      trustLevel.denominator * validatorSet.nodesPower(commit.forBlock.toList).power() >
        validatorSet.totalPower.power() * trustLevel.numerator
    }
  }

}
