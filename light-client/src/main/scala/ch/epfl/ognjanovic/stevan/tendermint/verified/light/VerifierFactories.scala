package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.hashing.Hashers.DefaultHasher
import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.DefaultCommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.ExpirationCheckerFactories.{
  ExpirationCheckerConfiguration,
  ExpirationCheckerFactory
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier

object VerifierFactories {

  trait VerifierFactory {

    def constructInstance(
      votingPowerVerifier: VotingPowerVerifier,
      expirationCheckerConfiguration: ExpirationCheckerConfiguration): Verifier

  }

  class DefaultVerifierFactory(expirationCheckerFactory: ExpirationCheckerFactory) extends VerifierFactory {

    override def constructInstance(
      votingPowerVerifier: VotingPowerVerifier,
      expirationCheckerConfiguration: ExpirationCheckerConfiguration): Verifier = {
      val expirationChecker = expirationCheckerFactory.constructChecker(expirationCheckerConfiguration)

      val verifier = DefaultTrustVerifier()
      val commitSignatureVerifier = new DefaultCommitSignatureVerifier()

      val commitValidator = DefaultCommitValidator(votingPowerVerifier, commitSignatureVerifier)

      Verifier(
        DefaultLightBlockValidator(expirationChecker, commitValidator, new DefaultHasher(MerkleRoot.default())),
        verifier,
        commitValidator
      )
    }

  }

}
