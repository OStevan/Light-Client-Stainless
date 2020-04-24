package ch.epfl.ognjanovic.stevan.light

import ch.epfl.ognjanovic.stevan.types.Height
import ch.epfl.ognjanovic.stevan.types.SignedHeader.SignedHeader
import stainless.collection._
import stainless.lang._

object LightClient {

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

  case class WaitingForHeader(
    height: Height,
    trustedState: TrustedState,
    untrustedState: UntrustedState) extends VerifierState {
    require(height > trustedState.currentHeight() && untrustedStateHeightInvariant(height, untrustedState))

    def headerResponse(signedHeader: SignedHeader): (TrustedState, UntrustedState) = {
      require(signedHeader.header.height == height && signedHeader.header.height > trustedState.currentHeight())
      val newUntrustedState = untrustedState.addSignedHeader(signedHeader)
      (trustedState, newUntrustedState)
    }.ensuring(res => untrustedStateHeightInvariant(res._1.currentHeight(), res._2))
  }

  case class VerifierStateMachine(verifierState: VerifierState) {
    def processMessage(message: Message): VerifierStateMachine = (verifierState, message) match {
      case (InitialState, verificationRequest: VerificationRequest) =>
        val trustedSignedHeader = verificationRequest.trustedSignedHeader
        val signedHeaderToVerify = verificationRequest.signedHeaderToVerify
        if (trustedSignedHeader.isExpired())
          VerifierStateMachine(
            Finished(verdict = false, TrustedState(trustedSignedHeader), UntrustedState(signedHeaderToVerify)))
        else if (signedHeaderToVerify.header.height <= trustedSignedHeader.header.height)
          VerifierStateMachine(
            Finished(verdict = true, TrustedState(trustedSignedHeader), UntrustedState.empty))
        else
          VerifierStateMachine(
            verify(TrustedState(trustedSignedHeader), UntrustedState(signedHeaderToVerify)))

      case (state: WaitingForHeader, headerResponse: HeaderResponse)
        if state.height == headerResponse.signedHeader.header.height =>
        val (trustedState, untrustedState) = state.headerResponse(headerResponse.signedHeader)
        val nextState = verify(trustedState, untrustedState)
        VerifierStateMachine(nextState)

      case _ => this // transitions to ignore
    }

    @scala.annotation.tailrec
    private def verify(trustedState: TrustedState, untrustedState: UntrustedState): VerifierState = {
      require(untrustedStateHeightInvariant(trustedState.currentHeight(), untrustedState))
      untrustedState.removeHead match {
        case (None(), emptyUntrustedState) => Finished(verdict = true, trustedState, emptyUntrustedState)

        case (Some(nextToVerify), newUntrustedState) =>
          if (trustedState.isAdjacent(nextToVerify)) {
            if (trustedState.adjacentHeaderTrust(nextToVerify))
              verify(trustedState.increaseTrust(nextToVerify), newUntrustedState)
            else
              Finished(verdict = false, trustedState, untrustedState)
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
