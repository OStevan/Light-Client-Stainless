package ch.epfl.ognjanovic.stevan.verified.integration

import ch.epfl.ognjanovic.stevan.verified.blockchain.BlockchainStates.BlockchainState
import ch.epfl.ognjanovic.stevan.verified.light.LightClient._
import ch.epfl.ognjanovic.stevan.verified.light.{LightClient, SoundSignedHeaderProvider, TrustedState, UntrustedState}
import ch.epfl.ognjanovic.stevan.verified.types.Height
import ch.epfl.ognjanovic.stevan.verified.types.SignedHeader.SignedHeader
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
