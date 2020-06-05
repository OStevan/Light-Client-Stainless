package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.{InvalidCommit, VerificationError}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.lang._

object CommitValidators {

  abstract class CommitValidator {
    def isCommitInvalid(untrustedLightBlock: LightBlock): Either[Unit, VerificationError]
  }

  case class DefaultCommitValidator(votingPowerVerifier: VotingPowerVerifier) extends CommitValidator {
    def isCommitInvalid(header: LightBlock): Either[Unit, VerificationError] = {
      if (header.commit.forBlock.nonEmpty &&
        (header.commit.forBlock subsetOf header.validatorSet.keys) &&
        header.commit.height == header.header.height &&
        votingPowerVerifier.consensusObtained(header.validatorSet, header.commit))
        Left(())
      else
        Right(InvalidCommit)
    }
  }

}
