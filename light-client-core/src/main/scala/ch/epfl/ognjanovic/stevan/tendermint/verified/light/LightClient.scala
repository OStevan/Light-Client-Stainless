package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.{inlineInvariant, opaque, pure}
import stainless.collection._
import stainless.lang.StaticChecks.Ensuring
import stainless.lang._

object LightClient {
  @inline
  def untrustedStateHeightInvariant(height: Height, untrustedState: UntrustedState): Boolean = {
    untrustedState.pending match {
      case list: Cons[LightBlock] => height < list.head.header.height
      case _: Nil[LightBlock] => true
    }
  }

  @inline
  def targetHeightInvariant(targetHeight: Height, untrustedState: List[LightBlock]): Boolean = {
    untrustedState.forall(_.header.height <= targetHeight)
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
    def verifySingle(trustedState: TrustedState, lightBlock: LightBlock): VerificationOutcome = {
      require(trustedState.currentHeight() < lightBlock.header.height)
      if (trustedState.trusted(lightBlock))
        checkCommit(lightBlock)
      else if (trustedState.isAdjacent(lightBlock))
        Failure
      else
        InsufficientTrust
    }.ensuring(res => (res == Success) ==> trustedState.trusted(lightBlock))

    def processHeader(
      waitingForHeader: WaitingForHeader,
      lightBlock: LightBlock): VerifierState = {
      require(lightBlock.header.height == waitingForHeader.requestHeight)
      stepByStepVerification(
        waitingForHeader.targetHeight,
        lightBlock,
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
    private def checkCommit(header: LightBlock): VerificationOutcome = {
      if (header.commit.signers.nonEmpty &&
        (header.commit.signers subsetOf header.validatorSet.keys) &&
        header.validatorSet.obtainedByzantineQuorum(header.commit.signers))
        Success
      else
        InvalidCommit
    }

    private def stepByStepVerification(
      targetHeight: Height,
      lightBlock: LightBlock,
      trustedState: TrustedState,
      untrustedState: UntrustedState): VerifierState = {
      require(
        lightBlock.header.height <= targetHeight &&
          trustedState.currentHeight() < lightBlock.header.height &&
          targetHeightInvariant(targetHeight, untrustedState.pending) &&
          untrustedStateHeightInvariant(lightBlock.header.height, untrustedState))
      verifySingle(trustedState, lightBlock) match {
        case Success =>
          untrustedState.pending match {
            case Cons(h, t) =>
              stepByStepVerification(targetHeight, h, trustedState.increaseTrust(lightBlock), UntrustedState(t))

            case Nil() =>
              val newTrustedState = trustedState.increaseTrust(lightBlock)
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
            trustedState.bisectionHeight(lightBlock.header.height),
            targetHeight,
            trustedState,
            untrustedState.addSignedHeader(lightBlock))

        case InvalidCommit =>
          Finished(InvalidCommit, trustedState, untrustedState.addSignedHeader(lightBlock))
        case Failure =>
          Finished(Failure, trustedState, untrustedState.addSignedHeader(lightBlock))
      }
    }.ensuring {
      case waitingForHeader: WaitingForHeader =>
        waitingForHeader.targetHeight == targetHeight &&
          waitingForHeader.trustedState.currentHeight() >= trustedState.currentHeight() &&
          (waitingForHeader.trustedState.currentHeight() > trustedState.currentHeight() ||
            waitingForHeader.requestHeight < lightBlock.header.height)
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
  }.ensuring(_ => untrustedState.pending.forall(_.header.height <= targetHeight))

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
