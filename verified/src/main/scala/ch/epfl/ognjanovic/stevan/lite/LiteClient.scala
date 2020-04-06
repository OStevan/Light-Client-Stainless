package ch.epfl.ognjanovic.stevan.lite

import ch.epfl.ognjanovic.stevan.types.SignedHeader
import stainless.lang._
import stainless.collection._
import ch.epfl.ognjanovic.stevan.types.Height

object LiteClient {

  private def pendingInvariant(pending: List[SignedHeader]): Boolean = pending match {
    case Cons(first, Cons(second, tail)) => first.header.height < second.header.height && pendingInvariant(Cons(second, tail))
    case _ => true
  }

  case class UntrustedState(pending: List[SignedHeader]) {
    require(pendingInvariant(pending))

    def addSignedHeader(signedHeader: SignedHeader): UntrustedState = pending match {
      case Nil() => UntrustedState(Cons(signedHeader, Nil()))
      case Cons(h, _) => 
        if (signedHeader.header.height < h.header.height)
          UntrustedState(Cons(signedHeader, pending))
        else
          this
    }

    def removeHead: (Option[SignedHeader], UntrustedState) = pending match {
      case Cons(h, t) => (Some(h), UntrustedState(t))
      case Nil() => (None(), this)
    }
  }

  abstract class Message
  case class VerificationRequest(trustedSignedHeader: SignedHeader, signedHeaderToVerify: SignedHeader) extends Message
  case class HeaderResponse(signedHeader: SignedHeader) extends Message

  abstract class VerifierState
  case object InitialState extends VerifierState
  case class Finished(verdict: Boolean, trustedState: TrustedState, untrustedState: UntrustedState) extends VerifierState
  case class WaitingForHeader(height: Height, trustedState: TrustedState, untrustedState: UntrustedState) extends VerifierState
  
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
        assert(state.height == headerResponse.signedHeader.header.height)
        VerifierStateMachine(
          verify(state.trustedState, state.untrustedState.addSignedHeader(headerResponse.signedHeader)),
          blockChainClient
        )
      case _ => this
    }

    private def verify(trustedState: TrustedState, untrustedState: UntrustedState): VerifierState = untrustedState.removeHead match {
      case (None(), emptyUntrustedState) => Finished(true, trustedState, emptyUntrustedState)
      case (Some(nextToVerify), newUntrustedState) =>
        if (trustedState.isAdjecent(nextToVerify)) {
          if (trustedState.adjecentHeaderTrust(nextToVerify))
            verify(trustedState.increaseTrust(nextToVerify), newUntrustedState)
          else
            Finished(false, trustedState, untrustedState)
        } else if (trustedState.nonAdjecentHeaderTrust(nextToVerify))
          verify(trustedState.increaseTrust(nextToVerify), newUntrustedState)
        else {
          val bisectionHeight: Height = trustedState.bisectionHeight(nextToVerify)
          blockChainClient.requestHeader(bisectionHeight)
          WaitingForHeader(bisectionHeight, trustedState, untrustedState)
        }
    }
  }
}
