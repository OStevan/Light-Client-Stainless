package ch.epfl.ognjanovic.stevan.tendermint.light

import java.nio.ByteBuffer
import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.DefaultCommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.BisectionHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.{ParameterizedTrustVerifier, TrustVerifier}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.{SimpleTrustedState, TrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.Verifiers.{DefaultVerifier, Verifier}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{DefaultLightBlockValidator, MultiStepVerifier}
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

  private def createDefaultVerifier(trustVerifier: TrustVerifier, trustingPeriod: Long, now: Instant): Verifier = {
    DefaultVerifier(new TimeBasedExpirationChecker(() => now, trustingPeriod), trustVerifier)
  }

  def deserializeMultiStepTestCase(path: String): (MultiStepVerifier, TrustedState, Height) = {
    val content = getContent(path)
    val (trustOptions: TrustOptions, primary: LightBlockProvider, heightToVerify: Height, now: Instant) =
      new CirceDeserializer(multiStepTestCaseDecoder)(content)

    val trustVerifier = ParameterizedTrustVerifier(trustOptions.trustLevel)
    val expirationChecker = new TimeBasedExpirationChecker(() => now, trustOptions.trustPeriod)
    val verifier =
      createDefaultVerifier(
        trustVerifier,
        trustOptions.trustPeriod,
        now)

    (
      MultiStepVerifier(
        primary,
        DefaultLightBlockValidator(expirationChecker, DefaultCommitValidator(trustVerifier)),
        verifier,
        BisectionHeightCalculator),
      SimpleTrustedState(primary.lightBlock(trustOptions.trustedHeight), trustVerifier),
      heightToVerify
    )
  }
}
