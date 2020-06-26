package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant
import java.time.temporal.ChronoUnit

import ch.epfl.ognjanovic.stevan.tendermint.hashing.Hashers.DefaultHasher
import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.rpc.TendermintSingleNodeContainer.Def
import ch.epfl.ognjanovic.stevan.tendermint.rpc.{RpcRequester, TendermintFullNodeClient, TendermintSingleNodeContainer}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.DefaultCommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.BisectionHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.SimpleTrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.InMemoryUntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{MultiStepVerifier, TrustLevel, Verifier}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.ParameterizedVotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height}
import com.dimafeng.testcontainers.scalatest.TestContainerForAll
import org.scalatest.flatspec.AnyFlatSpec
import sttp.client.HttpURLConnectionBackend

sealed class VerifierIntegrationTests extends AnyFlatSpec with TestContainerForAll {

  override val containerDef: Def = TendermintSingleNodeContainer.Def()

  override def afterContainersStart(containers: TendermintSingleNodeContainer): Unit = {
    super.afterContainersStart(containers)
    Thread.sleep(500)
  }

  private val lightBlockProviderFactory = new DefaultLightBlockProviderFactory()

  "Verification of a newest light block with a trusted state at height 1 " should "succeed" in withContainers {
    myContainer: TendermintSingleNodeContainer =>
      val timeOfTest = Instant.now()

      // Inside your test body you can do with your container whatever you want to
      val primary =
        lightBlockProviderFactory.constructProvider(secure = false, myContainer.url, Some(myContainer.rpcPort))

      val votingPowerVerifier = ParameterizedVotingPowerVerifier(TrustLevel(1, 3))

      val trustedState = SimpleTrustedState(primary.lightBlock(Height(1)), votingPowerVerifier)

      val expirationChecker = new TimeBasedExpirationChecker(
        () => timeOfTest,
        Duration(
          86400 +
            ChronoUnit.SECONDS.between(
              Instant.ofEpochSecond(
                trustedState.trustedLightBlock.header.time.seconds.toLong,
                trustedState.trustedLightBlock.header.time.nanos.toLong),
              Instant.now()
            ),
          0
        )
      )

      val verifier = DefaultTrustVerifier()
      val commitSignatureVerifier = new DefaultCommitSignatureVerifier()

      val commitValidator = DefaultCommitValidator(votingPowerVerifier, commitSignatureVerifier)

      val singleStepVerifier = Verifier(
        DefaultLightBlockValidator(expirationChecker, commitValidator, new DefaultHasher(MerkleRoot.default())),
        verifier,
        commitValidator
      )

      val multiStepVerifier = MultiStepVerifier(primary, singleStepVerifier, BisectionHeightCalculator)

      Thread.sleep(500)

      var heightToVerify = primary.currentHeight

      var result = multiStepVerifier.verifyUntrusted(
        trustedState,
        InMemoryUntrustedState(heightToVerify, stainless.collection.List.empty))

      assert(result.outcome.isLeft)
      assert(result.trustedState.currentHeight() == heightToVerify)
      assert(result.untrustedState.bottomHeight().isEmpty)
  }

  "Verifying one highest block with the state after verifying previous highest one" should "succeed" in withContainers {
    myContainer: TendermintSingleNodeContainer =>
      val timeOfTest = Instant.now()

      // Inside your test body you can do with your container whatever you want to
      val client =
        new TendermintFullNodeClient(false, myContainer.url, Some(myContainer.rpcPort), HttpURLConnectionBackend())

      val primary = new DefaultProvider("dockerchain", new RpcRequester(null, client))

      val votingPowerVerifier = ParameterizedVotingPowerVerifier(TrustLevel(1, 3))

      val trustedState = SimpleTrustedState(primary.lightBlock(Height(1)), votingPowerVerifier)

      val expirationChecker = new TimeBasedExpirationChecker(
        () => timeOfTest,
        Duration(
          86400 +
            ChronoUnit.SECONDS.between(
              Instant.ofEpochSecond(
                trustedState.trustedLightBlock.header.time.seconds.toLong,
                trustedState.trustedLightBlock.header.time.nanos.toLong),
              Instant.now()
            ),
          0
        )
      )

      val verifier = DefaultTrustVerifier()
      val commitSignatureVerifier = new DefaultCommitSignatureVerifier()

      val commitValidator = DefaultCommitValidator(votingPowerVerifier, commitSignatureVerifier)

      val singleStepVerifier = Verifier(
        DefaultLightBlockValidator(expirationChecker, commitValidator, new DefaultHasher(MerkleRoot.default())),
        verifier,
        commitValidator
      )

      val multiStepVerifier = MultiStepVerifier(primary, singleStepVerifier, BisectionHeightCalculator)

      Thread.sleep(500)

      var heightToVerify = primary.currentHeight

      var result = multiStepVerifier.verifyUntrusted(
        trustedState,
        InMemoryUntrustedState(heightToVerify, stainless.collection.List.empty))

      assert(result.outcome.isLeft)
      assert(result.trustedState.currentHeight() == heightToVerify)
      assert(result.untrustedState.bottomHeight().isEmpty)

      Thread.sleep(500)

      heightToVerify = primary.currentHeight

      result = multiStepVerifier.verifyUntrusted(
        result.trustedState,
        InMemoryUntrustedState(heightToVerify, stainless.collection.List.empty))

      assert(result.outcome.isLeft)
      assert(result.trustedState.currentHeight() == heightToVerify)
      assert(result.untrustedState.bottomHeight().isEmpty)
  }
}
