package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.hashing.Hashers.DefaultHasher
import ch.epfl.ognjanovic.stevan.tendermint.light.cases.MultiStepTestCase
import ch.epfl.ognjanovic.stevan.tendermint.light.EventLoopClient.EventLoopSupervisor
import ch.epfl.ognjanovic.stevan.tendermint.light.store.DefaultLightStoreFactory
import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStoreFactory.InMemoryLightStoreConfiguration
import ch.epfl.ognjanovic.stevan.tendermint.light.LightBlockStatuses.{Trusted, Verified}
import ch.epfl.ognjanovic.stevan.tendermint.light.Supervisor.{ForkDetected, NoWitnesses}
import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.rpc.Deserializer
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{DefaultTrustedStateFactory, VotingPowerVerifiers}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStateFactory.{
  LightStoreBackedTrustedStateConfiguration,
  SimpleTrustedStateConfiguration
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStateFactories.InMemoryUntrustedStateFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{LightBlock, PeerId}
import org.scalatest.flatspec.AnyFlatSpec

sealed class SupervisorForkDetectionTests extends AnyFlatSpec with VerifierTests {

  private val lightStoreFactory = new DefaultLightStoreFactory()

  private val untrustedStateFactory = new InMemoryUntrustedStateFactory()

  implicit private val testCaseDeserializer: Deserializer[MultiStepTestCase] =
    new CirceDeserializer(MultiStepTestCase.decoder)

  private val votingPowerVerifier = VotingPowerVerifiers.defaultVotingPowerVerifier

  private val trustedStateFactory = new DefaultTrustedStateFactory(lightStoreFactory)

  private val primaryTrustedStateBuilder
    : (LightBlock, VotingPowerVerifier) ⇒ (() ⇒ Iterable[LightBlock], TrustedState) = (lightBlock, verifier) ⇒ {
    val lightStore = lightStoreFactory.lightStore(InMemoryLightStoreConfiguration)
    lightStore.update(lightBlock, Trusted)

    val trustedState = trustedStateFactory.trustedState(
      LightStoreBackedTrustedStateConfiguration(lightBlock, verifier, Right(lightStore)))

    (() ⇒ lightStore.all(Verified), trustedState)
  }

  private val witnessTrustedStateBuilder =
    (lightBlock: LightBlock, votingPowerVerifier: VotingPowerVerifier) ⇒
      (_: PeerId) ⇒ {
        trustedStateFactory.trustedState(SimpleTrustedStateConfiguration(lightBlock, votingPowerVerifier))
      }

  "Receiving conflicting headers from witnesses" should "result in a failed synchronization" in {
    val (peerList, trustedState, expirationCheckerConfiguration, heightToVerify) =
      buildTest(VerifierTests.testCase("/bisection/multi-peer/conflicting_headers.json"))

    val lightStore = lightStoreFactory.lightStore(InMemoryLightStoreConfiguration)
    lightStore.update(trustedState.trustedLightBlock, Trusted)

    val supervisor = new EventLoopSupervisor(
      peerList,
      votingPowerVerifier,
      multiStepVerifierFactory,
      height ⇒ untrustedStateFactory.emptyWithTarget(height),
      expirationCheckerConfiguration,
      lightStore,
      new DefaultForkDetector(
        new DefaultHasher(MerkleRoot.default()),
        height ⇒ untrustedStateFactory.emptyWithTarget(height)),
      primaryTrustedStateBuilder,
      witnessTrustedStateBuilder
    )

    val result = supervisor.verifyToHeight(heightToVerify)

    assert(result.isRight && result.asInstanceOf[Right[LightBlock, Supervisor.Error]].value.isInstanceOf[ForkDetected])
  }

  "Conflicting commit from one of the witnesses" should "result in a failed synchronization" in {
    val (peerList, trustedState, expirationCheckerConfiguration, heightToVerify) =
      buildTest(VerifierTests.testCase("/bisection/multi-peer/conflicting_valid_commits_from_one_of_the_witnesses.json"))

    val lightStore = lightStoreFactory.lightStore(InMemoryLightStoreConfiguration)
    lightStore.update(trustedState.trustedLightBlock, Trusted)

    val supervisor = new EventLoopSupervisor(
      peerList,
      votingPowerVerifier,
      multiStepVerifierFactory,
      height ⇒ untrustedStateFactory.emptyWithTarget(height),
      expirationCheckerConfiguration,
      lightStore,
      new DefaultForkDetector(
        new DefaultHasher(MerkleRoot.default()),
        height ⇒ untrustedStateFactory.emptyWithTarget(height)),
      primaryTrustedStateBuilder,
      witnessTrustedStateBuilder
    )

    val result = supervisor.verifyToHeight(heightToVerify)

    assert(result.isRight && result.asInstanceOf[Right[LightBlock, Supervisor.Error]].value.isInstanceOf[ForkDetected])
  }

  "Conflicting commit from the only witnesses" should "result in a failed synchronization" in {
    val (peerList, trustedState, expirationCheckerConfiguration, heightToVerify) =
      buildTest(VerifierTests.testCase("/bisection/multi-peer/conflicting_valid_commits_from_the_only_witness.json"))

    val lightStore = lightStoreFactory.lightStore(InMemoryLightStoreConfiguration)
    lightStore.update(trustedState.trustedLightBlock, Trusted)

    val supervisor = new EventLoopSupervisor(
      peerList,
      votingPowerVerifier,
      multiStepVerifierFactory,
      height ⇒ untrustedStateFactory.emptyWithTarget(height),
      expirationCheckerConfiguration,
      lightStore,
      new DefaultForkDetector(
        new DefaultHasher(MerkleRoot.default()),
        height ⇒ untrustedStateFactory.emptyWithTarget(height)),
      primaryTrustedStateBuilder,
      witnessTrustedStateBuilder
    )

    val result = supervisor.verifyToHeight(heightToVerify)

    assert(result.isRight && result.asInstanceOf[Right[LightBlock, Supervisor.Error]].value.isInstanceOf[ForkDetected])
  }

  "Malicious validator set" should "result in a failed synchronization" in {
    val (peerList, trustedState, expirationCheckerConfiguration, heightToVerify) =
      buildTest(VerifierTests.testCase("/bisection/multi-peer/malicious_validator_set.json"))

    val lightStore = lightStoreFactory.lightStore(InMemoryLightStoreConfiguration)
    lightStore.update(trustedState.trustedLightBlock, Trusted)

    val supervisor = new EventLoopSupervisor(
      peerList,
      votingPowerVerifier,
      multiStepVerifierFactory,
      height ⇒ untrustedStateFactory.emptyWithTarget(height),
      expirationCheckerConfiguration,
      lightStore,
      new DefaultForkDetector(
        new DefaultHasher(MerkleRoot.default()),
        height ⇒ untrustedStateFactory.emptyWithTarget(height)),
      primaryTrustedStateBuilder,
      witnessTrustedStateBuilder
    )

    val result = supervisor.verifyToHeight(heightToVerify)

    assert(result.isRight && result.asInstanceOf[Right[LightBlock, Supervisor.Error]].value == NoWitnesses)
  }

}
