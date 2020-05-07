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

  sealed abstract class VerifierState

  case class Finished(
    verdict: Boolean,
    trustedState: TrustedState,
    untrustedState: UntrustedState) extends VerifierState {
    require((!verdict && untrustedState.pending.nonEmpty) || (verdict && untrustedState.pending.isEmpty))
  }

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

  case class VerifierStateMachine() {
    def processHeader(
      waitingForHeader: WaitingForHeader,
      signedHeader: SignedHeader): VerifierState = {
      val (trustedState, untrustedState) = waitingForHeader.headerResponse(signedHeader)
      verify(trustedState, untrustedState)
    }
//      .ensuring{ res =>
//      val previousTerminationMeasure = terminationMeasure(waitingForHeader)
//      val currentTerminationMeasure = terminationMeasure(res)
//      (previousTerminationMeasure._1 > currentTerminationMeasure._1) ||
//        (previousTerminationMeasure._1 == currentTerminationMeasure._1 &&
//          previousTerminationMeasure._2 > currentTerminationMeasure._2)
//    }

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

  def terminationMeasure(verifierState: VerifierState): (BigInt, BigInt) = {
    verifierState match {
      case WaitingForHeader(height, trustedState, untrustedState) =>
        val difference: BigInt = height.value - trustedState.currentHeight().value
        assert(difference > 0)
        if (untrustedState.pending.isEmpty)
          (difference, difference)
        else {
          val secondDiff: BigInt = untrustedState.pending.reverse.head.header.height.value - trustedState.currentHeight().value
          assert(secondDiff > 0)
          (secondDiff, difference)
        }
      case _: Finished => (BigInt(0), BigInt(0))
    }
  }.ensuring(res => (res._1 >= BigInt(0)) && (res._2 >= BigInt(0)))

}
