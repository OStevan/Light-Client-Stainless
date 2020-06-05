package ch.epfl.ognjanovic.stevan.tendermint.light

import java.nio.ByteBuffer
import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.DefaultCommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.BisectionHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.{SimpleTrustedState, TrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.{ParameterizedVotingPowerVerifier, VotingPowerVerifier}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{MultiStepVerifier, Verifier}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, Key, LightBlock, PeerId}
import io.circe.Decoder

import scala.io.Source

object MultiStepVerifierTests {
  val defaultProvider: PeerId =
    PeerId(
      Key(
        "tendermint/PubKeyEd25519",
        ByteBuffer.wrap("OAaNq3DX/15fGJP2MI6bujt1GRpvjwrqIevChirJsbc=".getBytes).asReadOnlyBuffer()))

  implicit val lightBlockDecoder: Decoder[LightBlock] = LightBlockDecoder.decoder(defaultProvider)

  def getContent(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString finally source.close()
  }

  private def createDefaultVerifier(
    votingPowerVerifier: VotingPowerVerifier,
    trustingPeriod: Long,
    now: Instant): Verifier = {
    val expirationChecker = new TimeBasedExpirationChecker(() => now, trustingPeriod)
    Verifier(
      DefaultLightBlockValidator(expirationChecker, DefaultCommitValidator(votingPowerVerifier)),
      DefaultTrustVerifier(),
      DefaultCommitValidator(votingPowerVerifier)
    )
  }

  def deserializeMultiStepTestCase(path: String): (MultiStepVerifier, TrustedState, Height) = {
    val content = getContent(path)
    val (trustOptions: TrustOptions, primary: LightBlockProvider, heightToVerify: Height, now: Instant) =
      new CirceDeserializer(multiStepTestCaseDecoder)(content)

    val trustVerifier = ParameterizedVotingPowerVerifier(trustOptions.trustLevel)
    val verifier =
      createDefaultVerifier(
        trustVerifier,
        trustOptions.trustPeriod,
        now)

    (
      MultiStepVerifier(
        primary,
        verifier,
        BisectionHeightCalculator),
      SimpleTrustedState(primary.lightBlock(trustOptions.trustedHeight), trustVerifier),
      heightToVerify
    )
  }
}
