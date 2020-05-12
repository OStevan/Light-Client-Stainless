package ch.epfl.ognjanovic.stevan.tendermint.verified.integration

import ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain.BlockchainStates.BlockchainState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClient._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{LightClient, SoundSignedHeaderProvider, TrustedState, UntrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.SignedHeader.SignedHeader
import stainless.collection._
import stainless.lang._

object ModelIntegration {
  def snapshotExecution(
    blockchainState: BlockchainState,
    trustedHeight: Height,
    heightToVerify: Height
  ): VerifierState = {
    require(blockchainState.currentHeight() > heightToVerify && heightToVerify > trustedHeight)
    val soundSignedHeaderProvider = SoundSignedHeaderProvider(blockchainState)
    val trustedSignedHeader = soundSignedHeaderProvider.getSignedHeader(trustedHeight)

    val trustedState = TrustedState(trustedSignedHeader)
    assert(trustedState.currentHeight() < heightToVerify)

    val verifier = WaitingForHeader(
      heightToVerify,
      trustedState,
      UntrustedState(Nil[SignedHeader]()))

    verify(verifier, soundSignedHeaderProvider, VerifierStateMachine())
  }

  @scala.annotation.tailrec
  def verify(
    waitingForHeader: WaitingForHeader,
    soundSignedHeaderProvider: SoundSignedHeaderProvider,
    verifier: VerifierStateMachine): Finished = {
    require(waitingForHeader.targetHeight() < soundSignedHeaderProvider.blockchainState.currentHeight())
    decreases(LightClient.terminationMeasure(waitingForHeader)._1, LightClient.terminationMeasure(waitingForHeader)._2)

    Height.helperLemma(
      waitingForHeader.height,
      waitingForHeader.targetHeight(),
      soundSignedHeaderProvider.blockchainState.currentHeight())

    verifier.processHeader(waitingForHeader, soundSignedHeaderProvider.getSignedHeader(waitingForHeader.height)) match {
      case state: WaitingForHeader => verify(state, soundSignedHeaderProvider, verifier)
      case state: Finished => state
    }
  }
}
