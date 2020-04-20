package ch.epfl.ognjanovic.stevan.integration

import ch.epfl.ognjanovic.stevan.blockchain.BlockchainStates.BlockchainState
import ch.epfl.ognjanovic.stevan.types.Height
import stainless.lang._
import stainless.annotation._
import ch.epfl.ognjanovic.stevan.types.SignedHeader
import ch.epfl.ognjanovic.stevan.lite.LiteClient._

object ModelIntegration {
  def snapshotExecution(blockchainState: BlockchainState, trustedHeight: Height, heightToVerify: Height): VerifierState = {
      require(blockchainState.currentHeight() > heightToVerify && trustedHeight < heightToVerify)
      val soundSignedHeaderProvider = SoundSignedHeaderProvider(blockchainState)
      val trustedSignedHeader = soundSignedHeaderProvider.getSignedHeader(trustedHeight)
      val headerToVerify = soundSignedHeaderProvider.getSignedHeader(heightToVerify)
      val verifier = VerifierStateMachine(InitialState)
      verify(soundSignedHeaderProvider, verifier, VerificationRequest(trustedSignedHeader, headerToVerify)).verifierState
  }.ensuring(res => res.isInstanceOf[Finished] || res.isInstanceOf[WaitingForHeader])

  def verify(soundSignedHeaderProvider: SoundSignedHeaderProvider, verifier: VerifierStateMachine, request: Message): VerifierStateMachine = {
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
      (res.header == blockchainState.header(height)) ||
      (res.commit.subsetOf(blockchainState.faulty) && res.header.height == height))
  }
}
