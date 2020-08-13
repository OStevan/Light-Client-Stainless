package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.hashing.Hashers.DefaultHasher
import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.DefaultCommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TimeValidatorFactories.{
  TimeValidatorConfig,
  TimeValidatorFactory
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier

object VerifierFactories {

  trait VerifierFactory {

    def constructInstance(votingPowerVerifier: VotingPowerVerifier, timeValidatorConfig: TimeValidatorConfig): Verifier

  }

  class DefaultVerifierFactory(timeValidatorFactory: TimeValidatorFactory) extends VerifierFactory {

    override def constructInstance(
      votingPowerVerifier: VotingPowerVerifier,
      timeValidatorConfig: TimeValidatorConfig): Verifier = {
      val timeValidator = timeValidatorFactory.constructChecker(timeValidatorConfig)

      val verifier = DefaultTrustVerifier()
      val commitSignatureVerifier = new DefaultCommitSignatureVerifier()

      val commitValidator = DefaultCommitValidator(votingPowerVerifier, commitSignatureVerifier)

      Verifier(
        DefaultLightBlockValidator(timeValidator, commitValidator, new DefaultHasher(MerkleRoot.default())),
        verifier,
        commitValidator
      )
    }

  }

}
