package ch.epfl.ognjanovic.stevan.tendermint.verified.integration

import ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain.BlockchainStates.BlockchainState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.{SimpleTrustedState, TrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.VerificationOutcome
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.Verifiers.DefaultVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types._
import stainless.annotation.pure

object ModelIntegration {
  def snapshotExecution(
    blockchainState: BlockchainState,
    trustedHeight: Height,
    heightToVerify: Height,
    nextHeightCalculator: NextHeightCalculator
  ): VerificationOutcome = {
    require(
      blockchainState.currentHeight() >= Height(2) &&
        blockchainState.currentHeight() > heightToVerify &&
        heightToVerify > trustedHeight)
    val soundSignedHeaderProvider = BlockchainLightBlockProviders(blockchainState)
    val trustedSignedHeader = soundSignedHeaderProvider.lightBlock(trustedHeight)

    val trustedState: TrustedState = SimpleTrustedState(trustedSignedHeader, TrustVerifiers.defaultTrustVerifier)
    val untrustedState = UntrustedStates.empty(heightToVerify)
    assert(untrustedState.bottomHeight().forall(heightToVerify < _))
    assert(trustedState.currentHeight() < heightToVerify)
    assert(heightToVerify <= untrustedState.targetLimit)

    MultiStepVerifier(
      soundSignedHeaderProvider,
      DefaultVerifier(
        HeightBasedExpirationChecker(blockchainState.blockchain.minTrustedHeight),
        TrustVerifiers.defaultTrustVerifier),
      nextHeightCalculator)
      .verifyUntrusted(trustedState, untrustedState)
  }

  private[integration] case class HeightBasedExpirationChecker(height: Height) extends ExpirationChecker {
    override def isExpired(lightBlock: LightBlock): Boolean = height > lightBlock.header.height
  }

  private[integration] case class BlockchainLightBlockProviders(
    blockchainState: BlockchainState) extends LightBlockProvider {
    require(blockchainState.currentHeight() >= Height(2))

    @pure
    override def lightBlock(height: Height): LightBlock = {
      require(height <= currentHeight)
      blockchainState.lightBlock(height)
    }

    @pure
    override def currentHeight: Height = {
      assert(blockchainState.currentHeight() >= Height(2)) // helps with verification
      Height(blockchainState.currentHeight().value - 1)
    }

    override def chainId: String = "verified-chain-01"
  }

}
