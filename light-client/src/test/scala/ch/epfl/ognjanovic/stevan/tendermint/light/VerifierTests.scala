package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant
import java.util.Base64

import ch.epfl.ognjanovic.stevan.tendermint.hashing.Hashers.DefaultHasher
import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{
  DefaultCommitSignatureVerifier,
  DefaultLightBlockValidator,
  TimeBasedExpirationChecker,
  Verifier
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.DefaultCommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Key, LightBlock, PeerId}
import io.circe.Decoder

import scala.io.Source

object VerifierTests {

  val defaultProvider: PeerId =
    PeerId(
      Key(
        "tendermint/PubKeyEd25519",
        Base64.getDecoder.decode("OAaNq3DX/15fGJP2MI6bujt1GRpvjwrqIevChirJsbc=".getBytes).toVector))

  implicit val lightBlockDecoder: Decoder[LightBlock] = LightBlockDecoder.decoder(defaultProvider)

  def content(path: String): String = {
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

  def deserializeSingleStepTestCase(path: String): (Verifier, TrustedState, LightBlockProvider) = {
    val content = VerifierTests.content(path)
    val (trustedState, trustVerifier, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = createDefaultVerifier(trustVerifier, trustingPeriod, now)

    (verifier, trustedState, provider)
  }

}
