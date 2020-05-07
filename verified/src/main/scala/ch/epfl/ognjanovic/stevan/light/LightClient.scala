package ch.epfl.ognjanovic.stevan.light

import ch.epfl.ognjanovic.stevan.types.Height
import ch.epfl.ognjanovic.stevan.types.SignedHeader.SignedHeader
import stainless.annotation.inlineInvariant
import stainless.collection._
import stainless.lang._

object LightClient {

  @inline
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

  case class Finished(verdict: Boolean, trustedState: TrustedState, untrustedState: UntrustedState) extends VerifierState

  @inlineInvariant
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
      decreases(untrustedState.pending.size)
      untrustedState.pending match {
        case Nil() => Finished(verdict = true, trustedState, untrustedState)

        case Cons(next, tail) =>
          if (trustedState.isAdjacent(next)) {
            if (trustedState.adjacentHeaderTrust(next))
              verify(trustedState.increaseTrust(next), UntrustedState(tail))
            else
              Finished(verdict = false, trustedState, untrustedState)
          } else if (trustedState.nonAdjacentHeaderTrust(next))
            verify(trustedState.increaseTrust(next), UntrustedState(tail))
          else {
            val bisectionHeight: Height = trustedState.bisectionHeight(next)
            WaitingForHeader(bisectionHeight, trustedState, untrustedState)
          }
      }
    }
  }

}
