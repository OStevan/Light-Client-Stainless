package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.SignedHeaders.SignedHeader
import stainless.annotation.{inlineInvariant, opaque, pure}
import stainless.collection._
import stainless.lang.StaticChecks.Ensuring
import stainless.lang._

object LightClient {
  @inline
  private def untrustedStateHeightInvariant(height: Height, untrustedState: UntrustedState): Boolean = {
    untrustedState.pending match {
      case list: Cons[SignedHeader] => height < list.head.header.height
      case _: Nil[SignedHeader] => true
    }
  }

  sealed abstract class VerificationOutcome

  case object Success extends VerificationOutcome

  case object InvalidCommit extends VerificationOutcome

  case object Failure extends VerificationOutcome

  sealed abstract class VerifierState

  @inlineInvariant
  case class Finished(
    outcome: VerificationOutcome,
    trustedState: TrustedState,
    untrustedState: UntrustedState) extends VerifierState {
    require(
      (outcome == Success && untrustedState.pending.isEmpty) ||
        (outcome != Success && untrustedState.pending.nonEmpty))
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

      val bisection = verifyInternal(trustedState, untrustedState)

      if (invalidCommit(signedHeader))
        Finished(InvalidCommit, trustedState, untrustedState)
      else
        bisection
    }.ensuring {
      case state: WaitingForHeader =>
        if (waitingForHeader.trustedState.currentHeight() == state.trustedState.currentHeight())
          sameTrustedStateTerminationMeasure(waitingForHeader, state)
        else
          improvedTrustedStateLemma(waitingForHeader, state)

        val previousTerminationMeasure = terminationMeasure(waitingForHeader)
        val currentTerminationMeasure = terminationMeasure(state)
        ((previousTerminationMeasure._1 > currentTerminationMeasure._1) ||
          (previousTerminationMeasure._1 == currentTerminationMeasure._1 &&
            previousTerminationMeasure._2 > currentTerminationMeasure._2)) &&
          state.targetHeight() == waitingForHeader.targetHeight()

      case _: Finished => true
    }

    private def invalidCommit(header: SignedHeader): Boolean = {
      header.commit.nonEmpty &&
        (header.commit subsetOf header.header.validatorSet.keys) &&
        header.header.validatorSet.obtainedByzantineQuorum(header.commit)
    }

    private def verifyInternal(trustedState: TrustedState, untrustedState: UntrustedState): VerifierState = {
      require(
        untrustedStateHeightInvariant(trustedState.currentHeight(), untrustedState) &&
          untrustedState.pending.nonEmpty)
      decreases(untrustedState.pending.size)

      untrustedState.pending match {
        case Cons(h, tail) =>
          if (trustedState.trusted(h) && tail.isEmpty)
            Finished(outcome = Success, trustedState.increaseTrust(h), UntrustedState(Nil[SignedHeader]()))
          else if (trustedState.trusted(h))
            verifyInternal(trustedState.increaseTrust(h), UntrustedState(tail))
          else if (trustedState.isAdjacent(h))
            Finished(outcome = Failure, trustedState, untrustedState)
          else
            WaitingForHeader(trustedState.bisectionHeight(h), trustedState, untrustedState)
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
