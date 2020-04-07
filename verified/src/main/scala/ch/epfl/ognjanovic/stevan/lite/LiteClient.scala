package ch.epfl.ognjanovic.stevan.lite

import ch.epfl.ognjanovic.stevan.types.SignedHeader
import stainless.lang._
import stainless.collection._
import ch.epfl.ognjanovic.stevan.types.Height

object LiteClient {

  private def untrustedStateHeightInvariant(height: Height, untrustedState: UntrustedState): Boolean = {
    untrustedState.pending match {
      case list: Cons[SignedHeader] => height < list.head.header.height
      case _: Nil[SignedHeader] => true
    }
  }

  abstract class Message
  case class VerificationRequest(trustedSignedHeader: SignedHeader, signedHeaderToVerify: SignedHeader) extends Message
  case class HeaderResponse(signedHeader: SignedHeader) extends Message

  abstract class VerifierState
  case object InitialState extends VerifierState
  case class Finished(verdict: Boolean, trustedState: TrustedState, untrustedState: UntrustedState) extends VerifierState
  case class WaitingForHeader(height: Height, trustedState: TrustedState, untrustedState: UntrustedState) extends VerifierState {
    require(height > trustedState.currentHeight() && untrustedStateHeightInvariant(height, untrustedState))
  }
  
  case class VerifierStateMachine(verifierState: VerifierState, blockChainClient: BlockchainClient) {
    def processMessage(message: Message): VerifierStateMachine = (verifierState, message) match {
      case (InitialState, verificationRequest: VerificationRequest) =>
        val trustedSignedHeader = verificationRequest.trustedSignedHeader
        val signedHeaderToVerify = verificationRequest.signedHeaderToVerify
        if (blockChainClient.expired(trustedSignedHeader))
          VerifierStateMachine(
            Finished(false, TrustedState(trustedSignedHeader), UntrustedState(Cons(signedHeaderToVerify, Nil()))),
            blockChainClient)
        else if (signedHeaderToVerify.header.height <= trustedSignedHeader.header.height)
          VerifierStateMachine(
            Finished(true, TrustedState(trustedSignedHeader), UntrustedState(Nil())),
            blockChainClient)
        else
          VerifierStateMachine(
            verify(TrustedState(trustedSignedHeader), UntrustedState(Cons(signedHeaderToVerify, Nil()))),
            blockChainClient)

      case (state: WaitingForHeader, headerResponse: HeaderResponse) =>
        if (state.height == headerResponse.signedHeader.header.height) {
          val newUntrustedState = state.untrustedState.addSignedHeader(headerResponse.signedHeader)
          if (untrustedStateHeightInvariant(state.trustedState.currentHeight(), newUntrustedState)) // needed for verification for now
            VerifierStateMachine(
              verify(state.trustedState, newUntrustedState),
              blockChainClient
            )
          else
            this
        } else
          this // needed for verification, ignore responses which are not what was asked for

      case _ => this
    }

    private def verify(trustedState: TrustedState, untrustedState: UntrustedState): VerifierState = {
      require(untrustedStateHeightInvariant(trustedState.currentHeight(), untrustedState))
      untrustedState.removeHead match {
      case (None(), emptyUntrustedState) => Finished(true, trustedState, emptyUntrustedState)

      case (Some(nextToVerify), newUntrustedState) =>
        if (trustedState.isAdjacent(nextToVerify)) {
          if (trustedState.adjacentHeaderTrust(nextToVerify))
            verify(trustedState.increaseTrust(nextToVerify), newUntrustedState)
          else
            Finished(false, trustedState, untrustedState)
        } else if (trustedState.nonAdjacentHeaderTrust(nextToVerify))
          verify(trustedState.increaseTrust(nextToVerify), newUntrustedState)
        else {
          val bisectionHeight: Height = trustedState.bisectionHeight(nextToVerify)
          WaitingForHeader(bisectionHeight, trustedState, untrustedState)
        }
      }
    }
  }
}
