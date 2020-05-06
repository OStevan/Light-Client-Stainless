package ch.epfl.ognjanovic.stevan.integration

import ch.epfl.ognjanovic.stevan.blockchain.BlockchainStates.BlockchainState
import ch.epfl.ognjanovic.stevan.light.LightClient._
import ch.epfl.ognjanovic.stevan.light.{TrustedState, UntrustedState}
import ch.epfl.ognjanovic.stevan.types.Height
import ch.epfl.ognjanovic.stevan.types.SignedHeader.SignedHeader
import stainless.annotation._
import stainless.collection._

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
    verify(soundSignedHeaderProvider, verifier, HeaderResponse(untrustedSignedHeader)).verifierState
  }

  @scala.annotation.tailrec
  def verify(
    soundSignedHeaderProvider: SoundSignedHeaderProvider,
    verifier: VerifierStateMachine,
    request: Message
  ): VerifierStateMachine = {
    val result = verifier.processMessage(request)
    result.verifierState match {
      case state: WaitingForHeader if state.height < soundSignedHeaderProvider.blockchainState.currentHeight =>
        val requestHeight = state.height
        val soundSignedHeader = soundSignedHeaderProvider.getSignedHeader(requestHeight)
        verify(soundSignedHeaderProvider, result, HeaderResponse(soundSignedHeader))
      case _ => result
    }
  }

  case class SoundSignedHeaderProvider(blockchainState: BlockchainState) {
    @extern
    def getSignedHeader(height: Height): SignedHeader = {
      require(height < blockchainState.currentHeight())
      blockchainState.signedHeader(height)
    }.ensuring(res =>
      (res.header == blockchainState.header(height) && res.header.height == height) ||
        (res.commit.subsetOf(blockchainState.faulty) && res.header.height == height))
  }

}
