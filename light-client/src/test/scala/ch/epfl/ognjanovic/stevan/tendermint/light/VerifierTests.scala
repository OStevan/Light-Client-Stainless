package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.light.cases.{MultiStepTestCase, SingleStepTestCase}
import ch.epfl.ognjanovic.stevan.tendermint.light.cases.MultiStepTestCase.WitnessInput
import ch.epfl.ognjanovic.stevan.tendermint.rpc.Deserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.fork.PeerList
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
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height, LightBlock, PeerId}

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

    val peerId = PeerId(singleStepTestCase.initial.next_validator_set.values.head.publicKey)

    val trustedState = SimpleTrustedState(
      LightBlock(
        singleStepTestCase.initial.signed_header.header,
        singleStepTestCase.initial.signed_header.commit,
        singleStepTestCase.initial.next_validator_set,
        singleStepTestCase.initial.next_validator_set,
        peerId
      ),
      votingPowerVerifier
    )

    (
      verifierFactory.constructInstance(votingPowerVerifier, expirationCheckerConfig),
      trustedState,
      InMemoryProvider.fromInput(
        singleStepTestCase.initial.signed_header.header.chainId,
        peerId,
        singleStepTestCase.input)
    )
  }

  def buildTest(multiStepTestCase: MultiStepTestCase)
    : (PeerList[PeerId, LightBlockProvider], SimpleTrustedState, ExpirationCheckerConfiguration, Height) = {
    val trustVerifier = ParameterizedVotingPowerVerifier(multiStepTestCase.trust_options.trustLevel)

    val peerId = PeerId(multiStepTestCase.primary.lite_blocks(0).validator_set.values.head.publicKey)
    val witnessIds =
      multiStepTestCase.primary.lite_blocks(0).validator_set.values.tail.map(vals ⇒ PeerId(vals.publicKey)).toScala

    val primary =
      InMemoryProvider.fromInput(multiStepTestCase.primary.chain_id, peerId, multiStepTestCase.primary.lite_blocks)

    val witnesses: Map[PeerId, LightBlockProvider] =
      witnessIds
        .zip(multiStepTestCase.witnesses.getOrElse(Array.empty))
        .map(pair ⇒ (pair._1, InMemoryProvider.fromInput(pair._2.value.chain_id, pair._1, pair._2.value.lite_blocks)))
        .toMap

    (
      PeerList.fromScala(witnesses.updated(peerId, primary), peerId, witnessIds, List.empty, List.empty),
      SimpleTrustedState(primary.lightBlock(multiStepTestCase.trust_options.trustedHeight), trustVerifier),
      TimeBasedExpirationCheckerConfig(() ⇒ multiStepTestCase.now, multiStepTestCase.trust_options.trustPeriod),
      Height(multiStepTestCase.height_to_verify)
    )
  }

}

object VerifierTests {

  private def content(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString
    finally source.close()
  }

  def testCase[T](path: String)(implicit deserializer: Deserializer[T]): T = deserializer(VerifierTests.content(path))

}
