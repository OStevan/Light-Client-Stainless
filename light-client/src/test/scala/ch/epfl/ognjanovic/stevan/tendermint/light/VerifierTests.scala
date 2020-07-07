package ch.epfl.ognjanovic.stevan.tendermint.light

import java.util.Base64

import ch.epfl.ognjanovic.stevan.tendermint.light.cases.{MultiStepTestCase, SingleStepTestCase}
import ch.epfl.ognjanovic.stevan.tendermint.rpc.Deserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.ExpirationCheckerFactories._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.MultiStepVerifierFactories.DefaultMultiStepVerifierFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.BisectionHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierFactories._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.{
  ParameterizedVotingPowerVerifier,
  VotingPowerVerifier
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height, Key, LightBlock, PeerId}
import io.circe.Decoder

import scala.io.Source

trait VerifierTests {
  def verifierFactory: VerifierFactory = new DefaultVerifierFactory(DefaultExpirationCheckerFactory)

  def multiStepVerifierFactory = new DefaultMultiStepVerifierFactory(
    new DefaultVerifierFactory(DefaultExpirationCheckerFactory),
    BisectionHeightCalculator)

  def buildTest(
    singleStepTestCase: SingleStepTestCase,
    votingPowerVerifier: VotingPowerVerifier): (Verifier, TrustedState, LightBlockProvider) = {
    val expirationCheckerConfig = TimeBasedExpirationCheckerConfig(
      () ⇒ singleStepTestCase.initial.now,
      Duration(0, singleStepTestCase.initial.trusting_period))

    val trustedState = SimpleTrustedState(
      LightBlock(
        singleStepTestCase.initial.signed_header.header,
        singleStepTestCase.initial.signed_header.commit,
        singleStepTestCase.initial.next_validator_set,
        singleStepTestCase.initial.next_validator_set,
        VerifierTests.defaultProvider
      ),
      votingPowerVerifier
    )

    (
      verifierFactory.constructInstance(votingPowerVerifier, expirationCheckerConfig),
      trustedState,
      singleStepTestCase.input
    )
  }

  def buildTest(
    multiStepTestCase: MultiStepTestCase,
    votingPowerVerifier: VotingPowerVerifier): (MultiStepVerifier, SimpleTrustedState, Height) = {
    val trustVerifier = ParameterizedVotingPowerVerifier(multiStepTestCase.trust_options.trustLevel)
    val expirationCheckerConfig =
      TimeBasedExpirationCheckerConfig(() ⇒ multiStepTestCase.now, multiStepTestCase.trust_options.trustPeriod)

    (
      multiStepVerifierFactory.constructVerifier(multiStepTestCase.primary, votingPowerVerifier, expirationCheckerConfig),
      SimpleTrustedState(
        multiStepTestCase.primary.lightBlock(multiStepTestCase.trust_options.trustedHeight),
        trustVerifier),
      Height(multiStepTestCase.height_to_verify)
    )
  }

}

object VerifierTests {

  val defaultProvider: PeerId =
    PeerId(
      Key(
        "tendermint/PubKeyEd25519",
        Base64.getDecoder.decode("OAaNq3DX/15fGJP2MI6bujt1GRpvjwrqIevChirJsbc=".getBytes).toVector))

  implicit val lightBlockDecoder: Decoder[LightBlock] = LightBlockDecoder.decoder(defaultProvider)

  private def content(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString
    finally source.close()
  }

  def testCase[T](path: String)(implicit deserializer: Deserializer[T]): T = deserializer(VerifierTests.content(path))

}
