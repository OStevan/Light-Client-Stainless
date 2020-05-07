package ch.epfl.ognjanovic.stevan.integration

import ch.epfl.ognjanovic.stevan.blockchain.BlockchainStates.BlockchainState
import ch.epfl.ognjanovic.stevan.light.LightClient._
import ch.epfl.ognjanovic.stevan.light.{LightClient, SoundSignedHeaderProvider, TrustedState, UntrustedState}
import ch.epfl.ognjanovic.stevan.types.Height
import ch.epfl.ognjanovic.stevan.types.SignedHeader.SignedHeader
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
    assert(heightToVerify > trustedState.currentHeight())

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
    require(waitingForHeader.height < soundSignedHeaderProvider.blockchainState.currentHeight())
//    decreases(LightClient.terminationMeasure(waitingForHeader))
    verifier.processHeader(waitingForHeader, soundSignedHeaderProvider.getSignedHeader(waitingForHeader.height)) match {
      case state: WaitingForHeader => verify(state, soundSignedHeaderProvider, verifier)
      case state: Finished => state
    }
  }
}
