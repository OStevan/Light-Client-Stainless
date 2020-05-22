package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.SignedHeaders.SignedHeader
import stainless.annotation.{inlineInvariant, opaque, pure}
import stainless.collection._
import stainless.lang.StaticChecks.Ensuring
import stainless.lang._

object LightClient {
  @inline
  def untrustedStateHeightInvariant(height: Height, untrustedState: UntrustedState): Boolean = {
    untrustedState.pending match {
      case list: Cons[SignedHeader] => height < list.head.header.header.height
      case _: Nil[SignedHeader] => true
    }
  }

  @inline
  def targetHeightInvariant(targetHeight: Height, untrustedState: List[SignedHeader]): Boolean = {
    untrustedState.forall(_.header.header.height <= targetHeight)
  }

  sealed abstract class VerificationOutcome

  case object Success extends VerificationOutcome

  case object InvalidCommit extends VerificationOutcome

  case object Failure extends VerificationOutcome

  case object InsufficientTrust extends VerificationOutcome

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
    requestHeight: Height,
    targetHeight: Height,
    trustedState: TrustedState,
    untrustedState: UntrustedState) extends VerifierState {
    require(
      requestHeight <= targetHeight &&
        trustedState.currentHeight() < requestHeight &&
        untrustedStateHeightInvariant(requestHeight, untrustedState) &&
        targetHeightInvariant(targetHeight, untrustedState.pending))
  }

  case class VerifierStateMachine() {

    @pure
    def verifySingle(trustedState: TrustedState, signedHeader: SignedHeader): VerificationOutcome = {
      require(trustedState.currentHeight() < signedHeader.header.header.height)
      if (trustedState.trusted(signedHeader))
        checkCommit(signedHeader)
      else if (trustedState.isAdjacent(signedHeader))
        Failure
      else
        InsufficientTrust
    }.ensuring(res => (res == Success) ==> trustedState.trusted(signedHeader))

    def processHeader(
      waitingForHeader: WaitingForHeader,
      signedHeader: SignedHeader): VerifierState = {
      require(signedHeader.header.header.height == waitingForHeader.requestHeight)
      stepByStepVerification(
        waitingForHeader.targetHeight,
        signedHeader,
        waitingForHeader.trustedState,
        waitingForHeader.untrustedState)
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
          state.targetHeight == waitingForHeader.targetHeight

      case _: Finished => true
    }

    @pure
    private def checkCommit(header: SignedHeader): VerificationOutcome = {
      if (header.commit.signers.nonEmpty &&
        (header.commit.signers subsetOf header.header.validatorSet.keys) &&
        header.header.validatorSet.obtainedByzantineQuorum(header.commit.signers))
        Success
      else
        InvalidCommit
    }

    private def stepByStepVerification(
      targetHeight: Height,
      signedHeader: SignedHeader,
      trustedState: TrustedState,
      untrustedState: UntrustedState): VerifierState = {
      require(
        signedHeader.header.header.height <= targetHeight &&
          trustedState.currentHeight() < signedHeader.header.header.height &&
          targetHeightInvariant(targetHeight, untrustedState.pending) &&
          untrustedStateHeightInvariant(signedHeader.header.header.height, untrustedState))
      verifySingle(trustedState, signedHeader) match {
        case Success =>
          untrustedState.pending match {
            case Cons(h, t) =>
              stepByStepVerification(targetHeight, h, trustedState.increaseTrust(signedHeader), UntrustedState(t))

            case Nil() =>
              val newTrustedState = trustedState.increaseTrust(signedHeader)
              if (newTrustedState.currentHeight() == targetHeight)
                Finished(Success, newTrustedState, untrustedState)
              else if (newTrustedState.currentHeight() + 1 == targetHeight)
                WaitingForHeader(
                  targetHeight,
                  targetHeight,
                  newTrustedState,
                  untrustedState)
              else
                WaitingForHeader(
                  newTrustedState.bisectionHeight(targetHeight),
                  targetHeight,
                  newTrustedState,
                  untrustedState)
          }

        case InsufficientTrust =>
          WaitingForHeader(
            trustedState.bisectionHeight(signedHeader.header.header.height),
            targetHeight,
            trustedState,
            untrustedState.addSignedHeader(signedHeader))

        case InvalidCommit =>
          Finished(InvalidCommit, trustedState, untrustedState.addSignedHeader(signedHeader))
        case Failure =>
          Finished(Failure, trustedState, untrustedState.addSignedHeader(signedHeader))
      }
    }.ensuring {
      case waitingForHeader: WaitingForHeader =>
        waitingForHeader.targetHeight == targetHeight &&
          waitingForHeader.trustedState.currentHeight() >= trustedState.currentHeight() &&
          (waitingForHeader.trustedState.currentHeight() > trustedState.currentHeight() ||
            waitingForHeader.requestHeight < signedHeader.header.header.height)
      case _ => true
    }
  }

  @opaque
  def transitivityOfTargetHeight(targetHeight: Height, untrustedState: UntrustedState): Unit = {
    require(targetHeightInvariant(targetHeight, untrustedState.pending))
    untrustedState.pending match {
      case Nil() => ()
      case Cons(_, t) => transitivityOfTargetHeight(targetHeight, UntrustedState(t))
    }
  }.ensuring(_ => untrustedState.pending.forall(_.header.header.height <= targetHeight))

  @pure
  def terminationMeasure(waitingForHeader: WaitingForHeader): (BigInt, BigInt) = {
    val res: (BigInt, BigInt) = (
      waitingForHeader.targetHeight.value - waitingForHeader.trustedState.currentHeight().value,
      waitingForHeader.requestHeight.value - waitingForHeader.trustedState.currentHeight().value
    )
    res
  }.ensuring(res => res._1 >= BigInt(0) && res._2 >= BigInt(0))

  @opaque
  def sameTrustedStateTerminationMeasure(previous: WaitingForHeader, current: WaitingForHeader): Unit = {
    require(previous.targetHeight == current.targetHeight &&
      previous.trustedState.currentHeight() == current.trustedState.currentHeight() &&
      previous.requestHeight > current.requestHeight
    )
  }.ensuring { _ =>
    val previousTerminationMeasure = terminationMeasure(previous)
    val currentTerminationMeasure = terminationMeasure(current)

    previousTerminationMeasure._1 == currentTerminationMeasure._1 &&
      previousTerminationMeasure._2 > currentTerminationMeasure._2
  }

  @opaque
  def improvedTrustedStateLemma(previous: WaitingForHeader, current: WaitingForHeader): Unit = {
    require(previous.targetHeight == current.targetHeight &&
      previous.trustedState.currentHeight() < current.trustedState.currentHeight()
    )
  }.ensuring { _ =>
    val previousTerminationMeasure = terminationMeasure(previous)
    val currentTerminationMeasure = terminationMeasure(current)

    previousTerminationMeasure._1 > currentTerminationMeasure._1
  }
}
