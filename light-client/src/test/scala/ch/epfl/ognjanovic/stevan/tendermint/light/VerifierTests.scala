package ch.epfl.ognjanovic.stevan.tendermint.light

import java.nio.ByteBuffer
import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.DefaultCommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.Verifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Key, LightBlock, PeerId}
import io.circe.Decoder

import scala.io.Source

object VerifierTests {
  val defaultProvider: PeerId =
    PeerId(
      Key(
        "tendermint/PubKeyEd25519",
        ByteBuffer.wrap("OAaNq3DX/15fGJP2MI6bujt1GRpvjwrqIevChirJsbc=".getBytes).asReadOnlyBuffer()))

  implicit val lightBlockDecoder: Decoder[LightBlock] = LightBlockDecoder.decoder(defaultProvider)

  def content(path: String): String = {
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

  def deserializeSingleStepTestCase(path: String): (Verifier, TrustedState, LightBlockProvider) = {
    val content = VerifierTests.content(path)
    val (trustedState, trustVerifier, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = createDefaultVerifier(trustVerifier, trustingPeriod, now)

    (verifier, trustedState, provider)
  }
}