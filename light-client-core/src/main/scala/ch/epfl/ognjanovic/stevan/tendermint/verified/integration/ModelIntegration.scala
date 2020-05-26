package ch.epfl.ognjanovic.stevan.tendermint.verified.integration

import ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain.BlockchainStates.BlockchainState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types._
import stainless.annotation.pure
import stainless.lang._

object ModelIntegration {
  def snapshotExecution(
    blockchainState: BlockchainState,
    trustedHeight: Height,
    heightToVerify: Height
  ): VerifierState = {
    require(blockchainState.currentHeight() > heightToVerify && heightToVerify > trustedHeight)
    val soundSignedHeaderProvider = BlockchainLightBlockProviders(blockchainState)
    val trustedSignedHeader = soundSignedHeaderProvider.lightBlock(trustedHeight)

    val trustedState = TrustedState(trustedSignedHeader, DefaultTrustVerifier())
    assert(trustedState.currentHeight() < heightToVerify)
    assert(heightToVerify <= heightToVerify)
    assert(trustedState.currentHeight() < heightToVerify)
    assert(untrustedStateHeightInvariant(heightToVerify, UntrustedState.empty))
    assert(targetHeightInvariant(heightToVerify, UntrustedState.empty.pending))

    val verifier = WaitingForHeader(
      heightToVerify,
      heightToVerify,
      trustedState,
      UntrustedState.empty)

    verify(
      verifier,
      soundSignedHeaderProvider,
      Verifier(HeightBasedExpirationChecker(blockchainState.blockchain.minTrustedHeight), DefaultTrustVerifier()))
  }

  @scala.annotation.tailrec
  def verify(
    waitingForHeader: WaitingForHeader,
    lightBlockProvider: LightBlockProvider,
    verifier: Verifier): Finished = {
    require(waitingForHeader.targetHeight < lightBlockProvider.currentHeight)
    decreases(LightClientLemmas.terminationMeasure(waitingForHeader))
    Height.helperLemma(
      waitingForHeader.requestHeight,
      waitingForHeader.targetHeight,
      lightBlockProvider.currentHeight)

    verifier.processHeader(waitingForHeader, lightBlockProvider.lightBlock(waitingForHeader.requestHeight)) match {
      case state: WaitingForHeader => verify(state, lightBlockProvider, verifier)
      case state: Finished => state
    }
  }

  private [integration] case class HeightBasedExpirationChecker(height: Height) extends ExpirationChecker {
    override def isExpired(lightBlock: LightBlock): Boolean = height > lightBlock.header.height
  }

  private[integration] case class BlockchainLightBlockProviders(
    blockchainState: BlockchainState) extends LightBlockProvider {

    @pure
    override def lightBlock(height: Height): LightBlock = {
      require(height < blockchainState.currentHeight())
      blockchainState.lightBlock(height)
    }

    @pure
    override def currentHeight: Height = blockchainState.currentHeight()
  }

}
