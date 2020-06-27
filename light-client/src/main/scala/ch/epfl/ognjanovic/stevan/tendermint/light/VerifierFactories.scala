package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.hashing.Hashers.DefaultHasher
import ch.epfl.ognjanovic.stevan.tendermint.light.ExpirationCheckerFactories.ExpirationCheckerFactory
import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.DefaultCommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.Verifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Duration

object VerifierFactories {

  trait VerifierFactory {
    def constructInstance(votingPowerVerifier: VotingPowerVerifier, duration: Duration): Verifier
  }

  class DefaultVerifierFactory(expirationCheckerFactory: ExpirationCheckerFactory) extends VerifierFactory {

    override def constructInstance(votingPowerVerifier: VotingPowerVerifier, duration: Duration): Verifier = {
      val expirationChecker = expirationCheckerFactory.constructChecker(duration)

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
