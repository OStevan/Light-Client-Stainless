package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.FetchedStacks.UntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClientLemmas._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.VerifiedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.lang._
import stainless.lang.StaticChecks.Ensuring

case class MultiStepVerifier(
  lightBlockProvider: LightBlockProvider,
  verifier: Verifier,
  heightCalculator: NextHeightCalculator) {

  def verifyUntrusted(verifiedState: VerifiedState, untrustedState: UntrustedState): Finished = {
    require(
      verifiedState.currentHeight() <= untrustedState.targetLimit &&
        untrustedState.bottomHeight().isEmpty &&
        untrustedState.targetLimit <= lightBlockProvider.currentHeight)

    if (verifiedState.currentHeight() == untrustedState.targetLimit)
      Finished(Left(()), verifiedState, untrustedState)
    else {
      val nextHeight = untrustedState.targetLimit
      verify(WaitingForHeader(nextHeight, verifiedState, untrustedState))
    }
  }

  @scala.annotation.tailrec
  private def verify(waitingForHeader: WaitingForHeader): Finished = {
    require(waitingForHeader.untrustedState.targetLimit <= lightBlockProvider.currentHeight)
    decreases(LightClientLemmas.terminationMeasure(waitingForHeader))

    processHeader(waitingForHeader, lightBlockProvider.lightBlock(waitingForHeader.requestHeight)) match {
      case state: WaitingForHeader => verify(state)
      case state: Finished => state
    }
  }

  private def processHeader(waitingForHeader: WaitingForHeader, lightBlock: LightBlock): VerifierState = {
    require(lightBlock.header.height == waitingForHeader.requestHeight)

    stepByStepVerification(lightBlock, waitingForHeader.verifiedState, waitingForHeader.untrustedState)
  }.ensuring {
    case state: WaitingForHeader =>
      if (waitingForHeader.verifiedState.currentHeight() == state.verifiedState.currentHeight())
        sameVerifiedStateTerminationMeasure(waitingForHeader, state)
      else
        improvedVerifiedStateLemma(waitingForHeader, state)

      val previousTerminationMeasure = terminationMeasure(waitingForHeader)
      val currentTerminationMeasure = terminationMeasure(state)
      ((previousTerminationMeasure._1 > currentTerminationMeasure._1) ||
      (previousTerminationMeasure._1 == currentTerminationMeasure._1 &&
      previousTerminationMeasure._2 > currentTerminationMeasure._2)) &&
      state.untrustedState.targetLimit == waitingForHeader.untrustedState.targetLimit

    case _: Finished => true
  }

  private def stepByStepVerification(
    lightBlock: LightBlock,
    verifiedState: VerifiedState,
    untrustedState: UntrustedState): VerifierState = {
    require(
      lightBlock.header.height <= untrustedState.targetLimit &&
        verifiedState.currentHeight() < lightBlock.header.height &&
        untrustedState.bottomHeight().map(lightBlock.header.height < _).getOrElse(true))
    decreases(untrustedState.targetLimit.value - verifiedState.currentHeight().value)

    verifier.verify(verifiedState, lightBlock) match {
      case VerificationOutcomes.Success =>
        val newVerifiedState = verifiedState.increaseTrust(lightBlock)
        if (newVerifiedState.currentHeight() == untrustedState.targetLimit) {
          val result = Left[Unit, VerificationError](())
          Finished(result, newVerifiedState, untrustedState)
        } else if (untrustedState.hasNextHeader(newVerifiedState.currentHeight(), untrustedState.targetLimit)) {
          val (nextLightBlock, nextUntrustedState) = untrustedState.pop()
          stepByStepVerification(nextLightBlock, newVerifiedState, nextUntrustedState)
        } else if (newVerifiedState.currentHeight() + 1 == untrustedState.targetLimit)
          WaitingForHeader(untrustedState.targetLimit, newVerifiedState, untrustedState)
        else {
          val newTargetHeight = heightCalculator.nextHeight(
            newVerifiedState.currentHeight(),
            untrustedState.bottomHeight().getOrElse(untrustedState.targetLimit))
          WaitingForHeader(newTargetHeight, newVerifiedState, untrustedState)
        }

      case VerificationOutcomes.InsufficientTrust =>
        val nextHeight = heightCalculator.nextHeight(verifiedState.currentHeight(), lightBlock.header.height)
        val nextUntrustedState = untrustedState.push(lightBlock)
        WaitingForHeader(nextHeight, verifiedState, nextUntrustedState)

      case Failure(reason) =>
        val error = Right[Unit, VerificationError](reason)
        val newUntrustedState = untrustedState.push(lightBlock)

        Finished(error, verifiedState, newUntrustedState)
    }
  }.ensuring {
    case waitingForHeader: WaitingForHeader =>
      waitingForHeader.untrustedState.targetLimit == untrustedState.targetLimit &&
        waitingForHeader.verifiedState.currentHeight() >= verifiedState.currentHeight() &&
        (waitingForHeader.verifiedState.currentHeight() > verifiedState.currentHeight() ||
          waitingForHeader.requestHeight < lightBlock.header.height)
    case _ => true
  }

}
