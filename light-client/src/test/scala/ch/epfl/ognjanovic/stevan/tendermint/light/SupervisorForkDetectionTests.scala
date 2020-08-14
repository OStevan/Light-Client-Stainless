package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.hashing.Hashers.DefaultHasher
import ch.epfl.ognjanovic.stevan.tendermint.light.cases.MultiStepTestCase
import ch.epfl.ognjanovic.stevan.tendermint.light.EventLoopClient.EventLoopSupervisor
import ch.epfl.ognjanovic.stevan.tendermint.light.store.DefaultLightStoreFactory
import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStoreFactory.InMemoryLightStoreConfiguration
import ch.epfl.ognjanovic.stevan.tendermint.light.LightBlockStatuses.{Trusted, Verified}
import ch.epfl.ognjanovic.stevan.tendermint.light.Supervisor.ForkDetected
import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.rpc.Deserializer
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{DefaultVerifiedStateFactory, VotingPowerVerifiers}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.FetchedStackFactories.InMemoryFetchedStackFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStateFactory.{
  LightStoreBackedVerifiedStateConfiguration,
  SimpleVerifiedStateConfiguration
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.VerifiedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{LightBlock, PeerId}
import org.scalatest.flatspec.AnyFlatSpec

sealed class SupervisorForkDetectionTests extends AnyFlatSpec with VerifierTests {

  private val lightStoreFactory = new DefaultLightStoreFactory()

  private val fetchedStackFactory = new InMemoryFetchedStackFactory()

  implicit private val testCaseDeserializer: Deserializer[MultiStepTestCase] =
    new CirceDeserializer(MultiStepTestCase.decoder)

  private val votingPowerVerifier = VotingPowerVerifiers.defaultVotingPowerVerifier

  private val verifiedStateFactory = new DefaultVerifiedStateFactory(lightStoreFactory)

  private val primaryVerifiedStateBuilder: (LightBlock, VotingPowerVerifier) ⇒ (() ⇒ Iterable[LightBlock], VerifiedState) =
    (lightBlock, verifier) ⇒ {
      val lightStore = lightStoreFactory.lightStore(InMemoryLightStoreConfiguration)
      lightStore.update(lightBlock, Trusted)

      val verifiedState = verifiedStateFactory.verifiedState(
        LightStoreBackedVerifiedStateConfiguration(lightBlock, verifier, Right(lightStore)))

      (() ⇒ lightStore.all(Verified), verifiedState)
    }

  private val witnessVerifiedStateBuilder =
    (lightBlock: LightBlock, votingPowerVerifier: VotingPowerVerifier) ⇒
      (_: PeerId) ⇒ {
        verifiedStateFactory.verifiedState(SimpleVerifiedStateConfiguration(lightBlock, votingPowerVerifier))
      }

  "Receiving conflicting headers from witnesses" should "result in a failed synchronization" in {
    val (peerList, verifiedState, timeValidatorConfig, heightToVerify) =
      buildTest(VerifierTests.testCase("/bisection/multi-peer/conflicting_headers.json"))

    val lightStore = lightStoreFactory.lightStore(InMemoryLightStoreConfiguration)
    lightStore.update(verifiedState.verified, Trusted)

    val supervisor = new EventLoopSupervisor(
      peerList,
      votingPowerVerifier,
      multiStepVerifierFactory,
      height ⇒ fetchedStackFactory.emptyWithTarget(height),
      timeValidatorConfig,
      lightStore,
      new DefaultForkDetector(
        new DefaultHasher(MerkleRoot.default()),
        height ⇒ fetchedStackFactory.emptyWithTarget(height)),
      primaryVerifiedStateBuilder,
      witnessVerifiedStateBuilder
    )

    val result = supervisor.verifyToHeight(heightToVerify)

    assert(result.isRight && result.asInstanceOf[Right[LightBlock, Supervisor.Error]].value.isInstanceOf[ForkDetected])
  }

  "Conflicting commit from one of the witnesses" should "result in a failed synchronization" in {
    val (peerList, verifiedState, timeValidatorConfig, heightToVerify) =
      buildTest(VerifierTests.testCase("/bisection/multi-peer/conflicting_valid_commits_from_one_of_the_witnesses.json"))

    val lightStore = lightStoreFactory.lightStore(InMemoryLightStoreConfiguration)
    lightStore.update(verifiedState.verified, Trusted)

    val supervisor = new EventLoopSupervisor(
      peerList,
      votingPowerVerifier,
      multiStepVerifierFactory,
      height ⇒ fetchedStackFactory.emptyWithTarget(height),
      timeValidatorConfig,
      lightStore,
      new DefaultForkDetector(
        new DefaultHasher(MerkleRoot.default()),
        height ⇒ fetchedStackFactory.emptyWithTarget(height)),
      primaryVerifiedStateBuilder,
      witnessVerifiedStateBuilder
    )

    val result = supervisor.verifyToHeight(heightToVerify)

    assert(result.isRight && result.asInstanceOf[Right[LightBlock, Supervisor.Error]].value.isInstanceOf[ForkDetected])
  }

  "Conflicting commit from the only witnesses" should "result in a failed synchronization" in {
    val (peerList, verifiedState, timeValidatorConfig, heightToVerify) =
      buildTest(VerifierTests.testCase("/bisection/multi-peer/conflicting_valid_commits_from_the_only_witness.json"))

    val lightStore = lightStoreFactory.lightStore(InMemoryLightStoreConfiguration)
    lightStore.update(verifiedState.verified, Trusted)

    val supervisor = new EventLoopSupervisor(
      peerList,
      votingPowerVerifier,
      multiStepVerifierFactory,
      height ⇒ fetchedStackFactory.emptyWithTarget(height),
      timeValidatorConfig,
      lightStore,
      new DefaultForkDetector(
        new DefaultHasher(MerkleRoot.default()),
        height ⇒ fetchedStackFactory.emptyWithTarget(height)),
      primaryVerifiedStateBuilder,
      witnessVerifiedStateBuilder
    )

    val result = supervisor.verifyToHeight(heightToVerify)

    assert(result.isRight && result.asInstanceOf[Right[LightBlock, Supervisor.Error]].value.isInstanceOf[ForkDetected])
  }

  "Malicious validator set" should "result in a failed synchronization" in {
    val (peerList, verifiedState, timeValidatorConfig, heightToVerify) =
      buildTest(VerifierTests.testCase("/bisection/multi-peer/malicious_validator_set.json"))

    val lightStore = lightStoreFactory.lightStore(InMemoryLightStoreConfiguration)
    lightStore.update(verifiedState.verified, Trusted)

    val supervisor = new EventLoopSupervisor(
      peerList,
      votingPowerVerifier,
      multiStepVerifierFactory,
      height ⇒ fetchedStackFactory.emptyWithTarget(height),
      timeValidatorConfig,
      lightStore,
      new DefaultForkDetector(
        new DefaultHasher(MerkleRoot.default()),
        height ⇒ fetchedStackFactory.emptyWithTarget(height)),
      primaryVerifiedStateBuilder,
      witnessVerifiedStateBuilder
    )

    val result = supervisor.verifyToHeight(heightToVerify)

    assert(result.isRight && result.asInstanceOf[Right[LightBlock, Supervisor.Error]].value.isInstanceOf[ForkDetected])
  }

}
