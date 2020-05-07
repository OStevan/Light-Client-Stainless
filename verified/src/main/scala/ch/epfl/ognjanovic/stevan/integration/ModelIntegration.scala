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
    val untrustedSignedHeader = soundSignedHeaderProvider.getSignedHeader(heightToVerify)

    val trustedState = TrustedState(trustedSignedHeader)
    assert(heightToVerify > trustedState.currentHeight())

    val verifier = VerifierStateMachine(
      WaitingForHeader(
        heightToVerify,
        trustedState,
        UntrustedState(Nil[SignedHeader]())))
    verify(soundSignedHeaderProvider, verifier, untrustedSignedHeader).verifierState
  }

  @scala.annotation.tailrec
  def verify(
    soundSignedHeaderProvider: SoundSignedHeaderProvider,
    verifier: VerifierStateMachine,
    signedHeader: SignedHeader
  ): VerifierStateMachine = {
    decreases(LightClient.terminationMeasure(verifier.verifierState))
    val result = verifier.processHeader(signedHeader)
    result.verifierState match {
      case state: WaitingForHeader if state.height < soundSignedHeaderProvider.blockchainState.currentHeight =>
        val requestHeight = state.height
        val soundSignedHeader = soundSignedHeaderProvider.getSignedHeader(requestHeight)
        verify(soundSignedHeaderProvider, result, soundSignedHeader)
      case _ => result
    }
  }
}
