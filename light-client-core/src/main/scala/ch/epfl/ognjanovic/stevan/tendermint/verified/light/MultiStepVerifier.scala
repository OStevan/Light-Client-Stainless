package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClientLemmas._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedTraces.UntrustedTrace
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.VerifiedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import stainless.lang._
import stainless.lang.StaticChecks.Ensuring

case class MultiStepVerifier(
  lightBlockProvider: LightBlockProvider,
  verifier: Verifier,
  heightCalculator: NextHeightCalculator) {

  def verifyUntrusted(verifiedState: VerifiedState, untrustedTrace: UntrustedTrace): Finished = {
    require(
      verifiedState.currentHeight() <= untrustedTrace.targetLimit &&
        untrustedTrace.bottomHeight().isEmpty &&
        untrustedTrace.targetLimit <= lightBlockProvider.currentHeight)

    if (verifiedState.currentHeight() == untrustedTrace.targetLimit)
      Finished(Left(()), verifiedState, untrustedTrace)
    else {
      val nextHeight = untrustedTrace.targetLimit
      verify(WaitingForHeader(nextHeight, verifiedState, untrustedTrace))
    }
  }

  @scala.annotation.tailrec
  private def verify(waitingForHeader: WaitingForHeader): Finished = {
    require(waitingForHeader.untrustedTrace.targetLimit <= lightBlockProvider.currentHeight)
    decreases(LightClientLemmas.terminationMeasure(waitingForHeader))

    processHeader(waitingForHeader) match {
      case state: WaitingForHeader => verify(state)
      case state: Finished => state
    }
  }

  private def processHeader(waitingForHeader: WaitingForHeader): VerifierState = {
    require(waitingForHeader.untrustedTrace.targetLimit <= lightBlockProvider.currentHeight)
    stepByStepVerification(
      waitingForHeader.requestHeight,
      waitingForHeader.verifiedState,
      waitingForHeader.untrustedTrace)
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
      state.untrustedTrace.targetLimit == waitingForHeader.untrustedTrace.targetLimit

    case _: Finished => true
  }

  private def stepByStepVerification(
    next: Height,
    verifiedState: VerifiedState,
    untrustedTrace: UntrustedTrace): VerifierState = {
    require(
      untrustedTrace.targetLimit <= lightBlockProvider.currentHeight &&
        next <= untrustedTrace.targetLimit &&
        verifiedState.currentHeight() < next &&
        untrustedTrace.bottomHeight().map(next < _).getOrElse(true))
    decreases(untrustedTrace.targetLimit.value - verifiedState.currentHeight().value)

    val lightBlock = lightBlockProvider.lightBlock(next)

    // without a caching light block provider this is extremely wasteful but the algorithm is simpler.
    verifier.verify(verifiedState, lightBlock) match {
      case VerificationOutcomes.Success =>
        val newVerifiedState = verifiedState.increaseTrust(lightBlock)
        if (newVerifiedState.currentHeight() == untrustedTrace.targetLimit) {
          val result = Left[Unit, VerificationError](())
          Finished(result, newVerifiedState, untrustedTrace)
        } else if (untrustedTrace.hasNextHeader(newVerifiedState.currentHeight(), untrustedTrace.targetLimit)) {
          val (nextLightBlock, nextUntrustedState) = untrustedTrace.removeBottom()
          stepByStepVerification(nextLightBlock, newVerifiedState, nextUntrustedState)
        } else if (newVerifiedState.currentHeight() + 1 == untrustedTrace.targetLimit)
          WaitingForHeader(untrustedTrace.targetLimit, newVerifiedState, untrustedTrace)
        else {
          val newTargetHeight = heightCalculator.nextHeight(
            newVerifiedState.currentHeight(),
            untrustedTrace.bottomHeight().getOrElse(untrustedTrace.targetLimit))
          WaitingForHeader(newTargetHeight, newVerifiedState, untrustedTrace)
        }

      case VerificationOutcomes.InsufficientTrust =>
        val nextHeight = heightCalculator.nextHeight(verifiedState.currentHeight(), next)
        val nextUntrustedState = untrustedTrace.insertLightBlock(next)
        WaitingForHeader(nextHeight, verifiedState, nextUntrustedState)

      case Failure(reason) =>
        val error = Right[Unit, VerificationError](reason)
        val newUntrustedState = untrustedTrace.insertLightBlock(next)

        Finished(error, verifiedState, newUntrustedState)
    }
  }.ensuring {
    case waitingForHeader: WaitingForHeader =>
      waitingForHeader.untrustedTrace.targetLimit == untrustedTrace.targetLimit &&
        waitingForHeader.verifiedState.currentHeight() >= verifiedState.currentHeight() &&
        (waitingForHeader.verifiedState.currentHeight() > verifiedState.currentHeight() ||
          waitingForHeader.requestHeight < next)
    case _ => true
  }

}
