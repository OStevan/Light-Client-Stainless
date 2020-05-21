package ch.epfl.ognjanovic.stevan.tendermint.verified.integration

import ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain.BlockchainStates.BlockchainState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{LightClient, TrustedState, UntrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClient._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.SignedHeaderProviders.SignedHeaderProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.SignedHeaders.SignedHeader
import stainless.annotation.pure
import stainless.lang._

private object ModelIntegration {
  def snapshotExecution(
    blockchainState: BlockchainState,
    trustedHeight: Height,
    heightToVerify: Height
  ): VerifierState = {
    require(blockchainState.currentHeight() > heightToVerify && heightToVerify > trustedHeight)
    val soundSignedHeaderProvider = BlockchainSignedHeaderProvider(blockchainState)
    val trustedSignedHeader = soundSignedHeaderProvider.signedHeader(trustedHeight)

    val trustedState = TrustedState(trustedSignedHeader)
    assert(trustedState.currentHeight() < heightToVerify)
    assert(heightToVerify <= heightToVerify)
    assert(trustedState.currentHeight() < heightToVerify)
    assert(LightClient.untrustedStateHeightInvariant(heightToVerify, UntrustedState.empty))
    assert(targetHeightInvariant(heightToVerify, UntrustedState.empty.pending))

    val verifier = WaitingForHeader(
      heightToVerify,
      heightToVerify,
      trustedState,
      UntrustedState.empty)

    verify(verifier, soundSignedHeaderProvider, VerifierStateMachine())
  }

  @scala.annotation.tailrec
  def verify(
    waitingForHeader: WaitingForHeader,
    signedHeaderProvider: SignedHeaderProvider,
    verifier: VerifierStateMachine): Finished = {
    require(waitingForHeader.targetHeight < signedHeaderProvider.currentHeight)
    decreases(LightClient.terminationMeasure(waitingForHeader)._1, LightClient.terminationMeasure(waitingForHeader)._2)

    Height.helperLemma(
      waitingForHeader.requestHeight,
      waitingForHeader.targetHeight,
      signedHeaderProvider.currentHeight)

    verifier.processHeader(waitingForHeader, signedHeaderProvider.signedHeader(waitingForHeader.requestHeight)) match {
      case state: WaitingForHeader => verify(state, signedHeaderProvider, verifier)
      case state: Finished => state
    }
  }

  private[integration] case class BlockchainSignedHeaderProvider(
    blockchainState: BlockchainState) extends SignedHeaderProvider {

    @pure
    override def signedHeader(height: Height): SignedHeader = {
      require(height < blockchainState.currentHeight())
      blockchainState.signedHeader(height)
    }

    @pure
    override def currentHeight: Height = blockchainState.currentHeight()
  }

}
