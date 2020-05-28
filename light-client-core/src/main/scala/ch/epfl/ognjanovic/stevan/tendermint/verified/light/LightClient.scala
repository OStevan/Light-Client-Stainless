package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClientLemmas._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.AbstractUntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.Verifiers.Verifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types._
import stainless.lang.StaticChecks.Ensuring

case class LightClient(
  verifier: Verifier,
  heightCalculator: NextHeightCalculator) {


  def processHeader(
    waitingForHeader: WaitingForHeader,
    lightBlock: LightBlock): VerifierState = {
    require(lightBlock.header.height == waitingForHeader.requestHeight)
    assert(lightBlock.header.height <= waitingForHeader.targetHeight)
    assert(waitingForHeader.trustedState.currentHeight() < lightBlock.header.height)
    assert(waitingForHeader.targetHeight == waitingForHeader.untrustedState.targetLimit)
    assert(waitingForHeader.untrustedState.bottomHeight().map(lightBlock.header.height < _).getOrElse(true))
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
    untrustedState: AbstractUntrustedState): VerifierState = {
    require(
      lightBlock.header.height <= targetHeight &&
        trustedState.currentHeight() < lightBlock.header.height &&
        targetHeight == untrustedState.targetLimit &&
        untrustedState.bottomHeight().map(lightBlock.header.height < _).getOrElse(true))
    verifier.verify(trustedState, lightBlock) match {
      case Success =>
        val newTrustedState = trustedState.increaseTrust(lightBlock)
        if (newTrustedState.currentHeight() == targetHeight) {
          assert(newTrustedState.currentHeight() == untrustedState.targetLimit)
          Finished(Success, newTrustedState, untrustedState)
        } else if (untrustedState.isIntermediateFetched(newTrustedState.currentHeight(), targetHeight)) {
          val (nextLightBlock, nextUntrustedState) = untrustedState.removeBottom()
          stepByStepVerification(targetHeight, nextLightBlock, newTrustedState, nextUntrustedState)
        } else if (newTrustedState.currentHeight() + 1 == targetHeight){
          assert(untrustedState.bottomHeight().isEmpty)

          WaitingForHeader(targetHeight, targetHeight, newTrustedState, untrustedState)
        } else {
          assert(newTrustedState.currentHeight() + 1 < targetHeight)
          assert(newTrustedState.currentHeight() + 1 < untrustedState.bottomHeight().getOrElse(targetHeight))
          WaitingForHeader(
            heightCalculator.nextHeight(
              newTrustedState.currentHeight(),
              untrustedState.bottomHeight().getOrElse(targetHeight)),
            targetHeight,
            newTrustedState,
            untrustedState)
        }

      case InsufficientTrust =>
        WaitingForHeader(
          heightCalculator.nextHeight(trustedState.currentHeight(), lightBlock.header.height),
          targetHeight,
          trustedState,
          untrustedState.insertLightBlock(lightBlock))

      case ExpiredTrustedState =>
        Finished(ExpiredTrustedState, trustedState, untrustedState.insertLightBlock(lightBlock))
      case InvalidCommit =>
        Finished(InvalidCommit, trustedState, untrustedState.insertLightBlock(lightBlock))
      case Failure =>
        Finished(Failure, trustedState, untrustedState.insertLightBlock(lightBlock))
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
