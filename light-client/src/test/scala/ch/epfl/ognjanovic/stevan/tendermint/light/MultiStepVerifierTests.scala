package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.hashing.Hashers.DefaultHasher
import ch.epfl.ognjanovic.stevan.tendermint.light.cases.TrustOptions
import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.DefaultCommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.BisectionHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.{SimpleTrustedState, TrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.{
  ParameterizedVotingPowerVerifier,
  VotingPowerVerifier
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height, LightBlock}
import io.circe.Decoder

import scala.io.Source

object MultiStepVerifierTests {

  implicit val lightBlockDecoder: Decoder[LightBlock] = LightBlockDecoder.decoder(VerifierTests.defaultProvider)

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
    val (trustOptions: TrustOptions, primary: LightBlockProvider, heightToVerify: Height, now: Instant) =
      new CirceDeserializer(multiStepTestCaseDecoder)(content)

    val trustVerifier = ParameterizedVotingPowerVerifier(trustOptions.trustLevel)
    val verifier =
      createDefaultVerifier(trustVerifier, trustOptions.trustPeriod, now)

    (
      MultiStepVerifier(primary, verifier, BisectionHeightCalculator),
      SimpleTrustedState(primary.lightBlock(trustOptions.trustedHeight), trustVerifier),
      heightToVerify
    )
  }

}
