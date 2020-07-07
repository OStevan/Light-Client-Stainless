package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.hashing.Hashers.DefaultHasher
import ch.epfl.ognjanovic.stevan.tendermint.light.cases.MultiStepTestCase
import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.DefaultCommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.BisectionHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.{SimpleTrustedState, TrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.{
  ParameterizedVotingPowerVerifier,
  VotingPowerVerifier
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height}

import scala.io.Source

object MultiStepVerifierTests {

  def getContent(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString
    finally source.close()
  }

  private def createDefaultVerifier(
    votingPowerVerifier: VotingPowerVerifier,
    trustingPeriod: Duration,
    now: Instant): Verifier = {
    val expirationChecker = new TimeBasedExpirationChecker(() => now, trustingPeriod)
    Verifier(
      DefaultLightBlockValidator(
        expirationChecker,
        DefaultCommitValidator(votingPowerVerifier, new DefaultCommitSignatureVerifier()),
        new DefaultHasher(MerkleRoot.default())),
      DefaultTrustVerifier(),
      DefaultCommitValidator(votingPowerVerifier, new DefaultCommitSignatureVerifier())
    )
  }

  def deserializeMultiStepTestCase(path: String): (MultiStepVerifier, TrustedState, Height) = {
    val content = getContent(path)
    val multiStepTestCase = new CirceDeserializer(MultiStepTestCase.decoder)(content)

    val trustVerifier = ParameterizedVotingPowerVerifier(multiStepTestCase.trust_options.trustLevel)
    val verifier =
      createDefaultVerifier(trustVerifier, multiStepTestCase.trust_options.trustPeriod, multiStepTestCase.now)

    (
      MultiStepVerifier(multiStepTestCase.primary, verifier, BisectionHeightCalculator),
      SimpleTrustedState(
        multiStepTestCase.primary.lightBlock(multiStepTestCase.trust_options.trustedHeight),
        trustVerifier),
      Height(multiStepTestCase.height_to_verify)
    )
  }

}
