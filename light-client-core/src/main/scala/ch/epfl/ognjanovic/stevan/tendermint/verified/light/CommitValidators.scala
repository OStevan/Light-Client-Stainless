package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.{InsufficientCommitPower, InvalidCommit, InvalidCommitVoteSignature, VerificationError}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.lang._

object CommitValidators {

  abstract class CommitValidator {
    def validateCommit(untrustedLightBlock: LightBlock): Either[Unit, VerificationError]

    def hasSufficientSignersOverlap(header: LightBlock): Either[Unit, VerificationError]
  }

  case class DefaultCommitValidator(
    votingPowerVerifier: VotingPowerVerifier,
    commitSignatureVerifier: CommitSignatureVerifier) extends CommitValidator {

    override def validateCommit(header: LightBlock): Either[Unit, VerificationError] = {
      if (header.commit.height == header.header.height)
        Left(())
      else
        Right(InvalidCommit)
    }

    override def hasSufficientSignersOverlap(header: LightBlock): Either[Unit, VerificationError] = {
      if (!(header.commit.forBlock.nonEmpty && (header.commit.forBlock subsetOf header.validatorSet.keys)))
        Right(InsufficientCommitPower)
      else if (!commitSignatureVerifier.verifyCommitSignatures(header))
        Right(InvalidCommitVoteSignature)
      else if (!votingPowerVerifier.consensusObtained(header.validatorSet, header.commit))
        Right(InsufficientCommitPower)
      else
        Left(())
    }
  }

}
