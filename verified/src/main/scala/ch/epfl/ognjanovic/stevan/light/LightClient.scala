package ch.epfl.ognjanovic.stevan.light

import ch.epfl.ognjanovic.stevan.types.Height
import ch.epfl.ognjanovic.stevan.types.SignedHeader.SignedHeader
import stainless.annotation.{inlineInvariant, opaque, pure}
import stainless.collection._
import stainless.lang.StaticChecks.Ensuring
import stainless.lang.StaticChecks.assert
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
    require(trustedState.currentHeight() < height && untrustedStateHeightInvariant(height, untrustedState))

    def headerResponse(signedHeader: SignedHeader): (TrustedState, UntrustedState) = {
      require(signedHeader.header.height == height && trustedState.currentHeight() < signedHeader.header.height)
      val newUntrustedState = untrustedState.addSignedHeader(signedHeader)
      (trustedState, newUntrustedState)
    }.ensuring(res => untrustedStateHeightInvariant(res._1.currentHeight(), res._2) &&
      res._2.pending.reverse.head.header.height == targetHeight())

    @pure
    def targetHeight(): Height = {
      if (untrustedState.pending.isEmpty)
        height
      else {
        val value = untrustedState.pending.reverse.head.header.height
        reverseLemma(height, untrustedState)
        value
      }
    }.ensuring(res => height <= res && trustedState.currentHeight() < res)
  }

  case class VerifierStateMachine() {
    def processHeader(
      waitingForHeader: WaitingForHeader,
      signedHeader: SignedHeader): VerifierState = {
      require(signedHeader.header.height == waitingForHeader.height)

      val (trustedState, untrustedState) = waitingForHeader.headerResponse(signedHeader)
      assert(waitingForHeader.targetHeight() == untrustedState.pending.reverse.head.header.height)
      assert(waitingForHeader.height == untrustedState.pending.head.header.height)
      verifyInternal(trustedState, untrustedState)
    }.ensuring {
      case state: WaitingForHeader =>
        assert(state.untrustedState.pending.nonEmpty)
        if (waitingForHeader.trustedState.currentHeight() == state.trustedState.currentHeight()) {
          assert(waitingForHeader.height > state.height)
          sameTrustedStateTerminationMeasure(waitingForHeader, state)
        } else {
          improvedTrustedStateLemma(waitingForHeader, state)
        }

        val previousTerminationMeasure = terminationMeasure(waitingForHeader)
        val currentTerminationMeasure = terminationMeasure(state)
        ((previousTerminationMeasure._1 > currentTerminationMeasure._1) ||
          (previousTerminationMeasure._1 == currentTerminationMeasure._1 &&
            previousTerminationMeasure._2 > currentTerminationMeasure._2)) &&
          state.targetHeight() == waitingForHeader.targetHeight()

      case _: Finished => true
    }

    private def verifyInternal(trustedState: TrustedState, untrustedState: UntrustedState): VerifierState = {
      require(untrustedStateHeightInvariant(trustedState.currentHeight(), untrustedState) &&
        untrustedState.pending.nonEmpty)
      decreases(untrustedState.pending.size)

      untrustedState.pending match {
        case Cons(h, tail) =>
          if (trustedState.trusted(h) && tail.isEmpty)
            Finished(verdict = true, trustedState.increaseTrust(h), UntrustedState(Nil[SignedHeader]()))
          else if (trustedState.trusted(h)) {
            assert(tail.nonEmpty)
            verifyInternal(trustedState.increaseTrust(h), UntrustedState(tail))
          } else if (trustedState.isAdjacent(h))
            Finished(verdict = false, trustedState, untrustedState)
          else {
            assert(!trustedState.isAdjacent(h) && !trustedState.trusted(h))
            val bisectionHeight: Height = trustedState.bisectionHeight(h)
            WaitingForHeader(bisectionHeight, trustedState, untrustedState)
          }
      }
    }.ensuring {
      case state: WaitingForHeader =>
        state.untrustedState.pending.nonEmpty &&
          untrustedState.pending.reverse.head.header.height == state.targetHeight() &&
          trustedState.currentHeight() <= state.trustedState.currentHeight() &&
          (
            (trustedState.currentHeight() == state.trustedState.currentHeight() &&
              state.height < state.untrustedState.pending.head.header.height &&
              state.untrustedState.pending == untrustedState.pending) ||
              trustedState.currentHeight() < state.trustedState.currentHeight())

      case _: Finished => true
    }
  }

  @pure
  def terminationMeasure(waitingForHeader: WaitingForHeader): (BigInt, BigInt) = {
    val res: (BigInt, BigInt) = (
      waitingForHeader.targetHeight().value - waitingForHeader.trustedState.currentHeight().value,
      waitingForHeader.height.value - waitingForHeader.trustedState.currentHeight().value
    )
    assert(res._1 > BigInt(0) && res._2 > BigInt(0))
    res
  }.ensuring(res => res._1 >= BigInt(0) && res._2 >= BigInt(0))

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

  @opaque
  def sameTrustedStateTerminationMeasure(previous: WaitingForHeader, current: WaitingForHeader): Unit = {
    require(previous.targetHeight() == current.targetHeight() &&
      previous.trustedState.currentHeight() == current.trustedState.currentHeight() &&
      previous.height > current.height
    )
  }.ensuring { _ =>
    val previousTerminationMeasure = terminationMeasure(previous)
    val currentTerminationMeasure = terminationMeasure(current)

    previousTerminationMeasure._1 == currentTerminationMeasure._1 &&
      previousTerminationMeasure._2 > currentTerminationMeasure._2
  }

  @opaque
  def improvedTrustedStateLemma(previous: WaitingForHeader, current: WaitingForHeader): Unit = {
    require(previous.targetHeight() == current.targetHeight() &&
      previous.trustedState.currentHeight() < current.trustedState.currentHeight()
    )
  }.ensuring { _ =>
    val previousTerminationMeasure = terminationMeasure(previous)
    val currentTerminationMeasure = terminationMeasure(current)

    previousTerminationMeasure._1 > currentTerminationMeasure._1
  }
}
