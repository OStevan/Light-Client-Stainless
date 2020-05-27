package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClientLemmas._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.Verifiers.Verifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types._
import stainless.collection.{Cons, Nil}
import stainless.lang.StaticChecks.Ensuring

case class LightClient(
  verifier: Verifier,
  heightCalculator: NextHeightCalculator) {


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
    verifier.verify(trustedState, lightBlock) match {
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
                heightCalculator.nextHeight(newTrustedState.currentHeight(), targetHeight),
                targetHeight,
                newTrustedState,
                untrustedState)
        }

      case InsufficientTrust =>
        WaitingForHeader(
          heightCalculator.nextHeight(trustedState.currentHeight(), lightBlock.header.height),
          targetHeight,
          trustedState,
          untrustedState.addSignedHeader(lightBlock))

      case ExpiredTrustedState =>
        Finished(ExpiredTrustedState, trustedState, untrustedState.addSignedHeader(lightBlock))
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
