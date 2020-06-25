package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant

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

  // To use containers in tests you need to use `withContainers` function
  it should "succeed to verify one latest light block with initial trusted height 1" in withContainers {
    myContainer: TendermintSingleNodeContainer =>
      // Inside your test body you can do with your container whatever you want to
      val client = new TendermintFullNodeClient(
        false,
        myContainer.url,
        Some(myContainer.rpcPort),
        HttpURLConnectionBackend())

      val primary = new DefaultProvider("dockerchain", new RpcRequester(null, client))

      val expirationChecker = new TimeBasedExpirationChecker(() => Instant.now(), Duration(86400, 0))

      val votingPowerVerifier = ParameterizedVotingPowerVerifier(TrustLevel(1, 3))

      val verifier = DefaultTrustVerifier()

      val singleStepVerifier = Verifier(
        DefaultLightBlockValidator(
          expirationChecker,
          DefaultCommitValidator(votingPowerVerifier, new DefaultCommitSignatureVerifier()),
          new DefaultHasher(MerkleRoot.default())),
        verifier,
        DefaultCommitValidator(votingPowerVerifier, new DefaultCommitSignatureVerifier())
      )

      val multiStepVerifier = MultiStepVerifier(
        primary,
        singleStepVerifier,
        BisectionHeightCalculator)

      val trustedState = SimpleTrustedState(primary.lightBlock(Height(1)), votingPowerVerifier)

      Thread.sleep(500)

      multiStepVerifier.verifyUntrusted(trustedState, InMemoryUntrustedState(primary.currentHeight, stainless.collection.List.empty))
  }
}
