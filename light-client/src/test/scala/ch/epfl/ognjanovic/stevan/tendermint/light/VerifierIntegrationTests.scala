package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant
import java.time.temporal.ChronoUnit

import ch.epfl.ognjanovic.stevan.tendermint.rpc.TendermintSingleNodeContainer
import ch.epfl.ognjanovic.stevan.tendermint.rpc.TendermintSingleNodeContainer.Def
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.ExpirationCheckerFactories.{
  DefaultExpirationCheckerFactory,
  TimeBasedExpirationCheckerConfig
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviderFactories.DefaultLightBlockProviderFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.MultiStepVerifierFactories.DefaultMultiStepVerifierFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.BisectionHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.SimpleTrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.InMemoryUntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierFactories.DefaultVerifierFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height}
import com.dimafeng.testcontainers.scalatest.TestContainerForAll
import org.scalatest.flatspec.AnyFlatSpec

sealed class VerifierIntegrationTests extends AnyFlatSpec with TestContainerForAll {

  override val containerDef: Def = TendermintSingleNodeContainer.Def()

  override def afterContainersStart(containers: TendermintSingleNodeContainer): Unit = {
    super.afterContainersStart(containers)
    Thread.sleep(500)
  }

  private val lightBlockProviderFactory = new DefaultLightBlockProviderFactory()
  private val expirationCheckerFactory = DefaultExpirationCheckerFactory
  private val verifierFactory = new DefaultVerifierFactory(expirationCheckerFactory)
  private val multiStepVerifierFactory = new DefaultMultiStepVerifierFactory(verifierFactory, BisectionHeightCalculator)

  "Verification of a newest light block with a trusted state at height 1 " should "succeed" in withContainers {
    myContainer: TendermintSingleNodeContainer =>
      val primary =
        lightBlockProviderFactory.constructProvider(secure = false, myContainer.url, Some(myContainer.rpcPort))
      val votingPowerVerifier = VotingPowerVerifiers.defaultTrustVerifier
      val trustedLightBlock = primary.lightBlock(Height(1))
      val now = Instant.now()
      val trustDuration =
        Duration(
          86400 +
            ChronoUnit.SECONDS.between(
              Instant.ofEpochSecond(
                trustedLightBlock.header.time.seconds.toLong,
                trustedLightBlock.header.time.nanos.toLong),
              Instant.now()
            ),
          0
        )

      val multiStepVerifier = multiStepVerifierFactory.constructVerifier(
        primary,
        votingPowerVerifier,
        TimeBasedExpirationCheckerConfig(() ⇒ now, trustDuration))
      val trustedState = SimpleTrustedState(trustedLightBlock, votingPowerVerifier)

      while (primary.currentHeight == Height(1)) {
        Thread.sleep(1000)
      }

      val heightToVerify = primary.currentHeight

      val result = multiStepVerifier.verifyUntrusted(
        trustedState,
        InMemoryUntrustedState(heightToVerify, stainless.collection.List.empty))

      assert(result.outcome.isLeft)
      assert(result.trustedState.currentHeight() == heightToVerify)
      assert(result.untrustedState.bottomHeight().isEmpty)
  }

  "Verifying one highest block with the state after verifying previous highest one" should "succeed" in withContainers {
    myContainer: TendermintSingleNodeContainer =>
      val primary =
        lightBlockProviderFactory.constructProvider(secure = false, myContainer.url, Some(myContainer.rpcPort))
      val votingPowerVerifier = VotingPowerVerifiers.defaultTrustVerifier
      val trustedLightBlock = primary.lightBlock(Height(1))
      val now = Instant.now()
      val trustDuration =
        Duration(
          86400 +
            ChronoUnit.SECONDS.between(
              Instant.ofEpochSecond(
                trustedLightBlock.header.time.seconds.toLong,
                trustedLightBlock.header.time.nanos.toLong),
              Instant.now()
            ),
          0
        )

      val multiStepVerifier = multiStepVerifierFactory.constructVerifier(
        primary,
        votingPowerVerifier,
        TimeBasedExpirationCheckerConfig(() ⇒ now, trustDuration))
      val trustedState = SimpleTrustedState(trustedLightBlock, votingPowerVerifier)

      while (primary.currentHeight == Height(1)) {
        Thread.sleep(1000)
      }
      var heightToVerify = primary.currentHeight

      var result = multiStepVerifier.verifyUntrusted(
        trustedState,
        InMemoryUntrustedState(heightToVerify, stainless.collection.List.empty))

      assert(result.outcome.isLeft)
      assert(result.trustedState.currentHeight() == heightToVerify)
      assert(result.untrustedState.bottomHeight().isEmpty)

      while (primary.currentHeight == result.trustedState.currentHeight()) {
        Thread.sleep(1000)
      }

      heightToVerify = primary.currentHeight

      result = multiStepVerifier.verifyUntrusted(
        result.trustedState,
        InMemoryUntrustedState(heightToVerify, stainless.collection.List.empty))

      assert(result.outcome.isLeft)
      assert(result.trustedState.currentHeight() == heightToVerify)
      assert(result.untrustedState.bottomHeight().isEmpty)
  }
}
