package ch.epfl.ognjanovic.stevan.light

import ch.epfl.ognjanovic.stevan.types.Height
import ch.epfl.ognjanovic.stevan.types.SignedHeader.SignedHeader
import stainless.annotation.{inlineInvariant, opaque, pure}
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
    }.ensuring(res => untrustedStateHeightInvariant(res._1.currentHeight(), res._2) &&
      res._2.pending.reverse.head.header.height == targetHeight())

    @pure
    def targetHeight(): Height = {
      if (untrustedState.pending.isEmpty)
        height
      else {
        reverseLemma(height, untrustedState)
        untrustedState.pending.reverse.head.header.height
      }
    }.ensuring(res => height <= res)
  }

  case class VerifierStateMachine() {
    def processHeader(
      waitingForHeader: WaitingForHeader,
      signedHeader: SignedHeader): VerifierState = {
      require(signedHeader.header.height == waitingForHeader.height)

      val (trustedState, untrustedState) = waitingForHeader.headerResponse(signedHeader)
      assert(waitingForHeader.targetHeight() == untrustedState.pending.reverse.head.header.height)
      verify(trustedState, untrustedState)
    }.ensuring{ res =>
      res match {
        case state: WaitingForHeader =>
          waitingForHeader.targetHeight() == state.targetHeight()
        case _: Finished => true
      }
    }
//      .ensuring{ res =>
//      val previousTerminationMeasure = terminationMeasure(waitingForHeader)
//      val currentTerminationMeasure = terminationMeasure(res)
//      (previousTerminationMeasure._1 > currentTerminationMeasure._1) ||
//        (previousTerminationMeasure._1 == currentTerminationMeasure._1 &&
//          previousTerminationMeasure._2 > currentTerminationMeasure._2)
//    }

    private def verify(trustedState: TrustedState, untrustedState: UntrustedState): VerifierState = {
      require(untrustedStateHeightInvariant(trustedState.currentHeight(), untrustedState) &&
        untrustedState.pending.nonEmpty)
      decreases(untrustedState.pending.size)

      untrustedState.pending match {
        case Cons(h, tail) =>
          if (trustedState.trusted(h) && tail.isEmpty)
            Finished(verdict = true, trustedState.increaseTrust(h), UntrustedState(Nil[SignedHeader]()))
          else if (trustedState.trusted(h)) {
            assert(tail.nonEmpty)
            verify(trustedState.increaseTrust(h), UntrustedState(tail))
          } else if (trustedState.isAdjacent(h))
            Finished(verdict = false, trustedState, untrustedState)
          else {
            assert(!trustedState.isAdjacent(h) && !trustedState.trusted(h))
            val bisectionHeight: Height = trustedState.bisectionHeight(h)
            WaitingForHeader(bisectionHeight, trustedState, untrustedState)
          }
      }
    }.ensuring{ res =>
      res match {
        case state: WaitingForHeader => untrustedState.pending.reverse.head.header.height == state.targetHeight()
        case _: Finished => true
      }
    }
  }

  def terminationMeasure(verifierState: VerifierState): (BigInt, BigInt) = {
    verifierState match {
      case WaitingForHeader(height, trustedState, untrustedState) =>
        val difference: BigInt = height.value - trustedState.currentHeight().value
        val measure: (BigInt, BigInt) = if (untrustedState.pending.isEmpty)
          (difference, difference)
        else {
          reverseLemma(trustedState.currentHeight(), untrustedState)
          val secondDiff = untrustedState.pending.reverse.head.header.height.value - trustedState.currentHeight().value
          (secondDiff, difference)
        }
        speedUpLemma(measure)
        measure

      case _: Finished => (BigInt(0), BigInt(0))
    }
  }.ensuring(res => (res._1 >= BigInt(0)) && (res._2 >= BigInt(0)))

  @opaque
  def speedUpLemma(pair: (BigInt, BigInt)): Unit = {
    require(pair._1 > BigInt(0) && pair._2 > BigInt(0))
  }.ensuring(_ => pair._1 >= BigInt(0) && pair._2 >= BigInt(0))

  @opaque
  def reverseLemma(height: Height, untrustedState: UntrustedState): Unit = {
    require(untrustedState.pending.nonEmpty && height < untrustedState.pending.head.header.height)
    untrustedState.pending match {
      case Cons(_, Nil()) => ()
      case Cons(_, t) => reverseLemma(height, UntrustedState(t))
    }
  }.ensuring(_ => height < untrustedState.pending.reverse.head.header.height)

}
