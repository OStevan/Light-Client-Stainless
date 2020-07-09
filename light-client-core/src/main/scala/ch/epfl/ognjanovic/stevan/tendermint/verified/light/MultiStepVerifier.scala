package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClientLemmas._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.VerifiedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.UntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
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

      assert(untrustedState.bottomHeight().map(nextHeight < _).getOrElse(true))
      assert(verifiedState.currentHeight() < nextHeight)
      assert(nextHeight <= untrustedState.targetLimit)
      verify(WaitingForHeader(nextHeight, verifiedState, untrustedState))
    }
  }

  @scala.annotation.tailrec
  private def verify(waitingForHeader: WaitingForHeader): Finished = {
    require(waitingForHeader.untrustedState.targetLimit <= lightBlockProvider.currentHeight)
    decreases(LightClientLemmas.terminationMeasure(waitingForHeader))

    processHeader(waitingForHeader) match {
      case state: WaitingForHeader => verify(state)
      case state: Finished => state
    }
  }

  private def processHeader(waitingForHeader: WaitingForHeader): VerifierState = {
    require(waitingForHeader.untrustedState.targetLimit <= lightBlockProvider.currentHeight)
    stepByStepVerification(
      waitingForHeader.requestHeight,
      waitingForHeader.verifiedState,
      waitingForHeader.untrustedState)
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
    next: Height,
    verifiedState: VerifiedState,
    untrustedState: UntrustedState): VerifierState = {
    require(
      untrustedState.targetLimit <= lightBlockProvider.currentHeight &&
        next <= untrustedState.targetLimit &&
        verifiedState.currentHeight() < next &&
        untrustedState.bottomHeight().map(next < _).getOrElse(true))
    decreases(untrustedState.targetLimit.value - verifiedState.currentHeight().value)

    val lightBlock = lightBlockProvider.lightBlock(next)
    assert(lightBlock.header.height == next)

    // without a caching light block provider this is extremely wasteful but the algorithm is simpler.
    verifier.verify(verifiedState, lightBlock) match {
      case VerificationOutcomes.Success =>
        val newVerifiedState = verifiedState.increaseTrust(lightBlock)
        if (newVerifiedState.currentHeight() == untrustedState.targetLimit) {
          val result = Left[Unit, VerificationError](())
          Finished(result, newVerifiedState, untrustedState)
        } else if (untrustedState.hasNextHeader(newVerifiedState.currentHeight(), untrustedState.targetLimit)) {
          val (nextLightBlock, nextUntrustedState) = untrustedState.removeBottom()
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
        val nextHeight = heightCalculator.nextHeight(verifiedState.currentHeight(), next)
        val nextUntrustedState = untrustedState.insertLightBlock(next)
        WaitingForHeader(nextHeight, verifiedState, nextUntrustedState)

      case Failure(reason) =>
        val error = Right[Unit, VerificationError](reason)
        val newUntrustedState = untrustedState.insertLightBlock(next)

        Finished(error, verifiedState, newUntrustedState)
    }
  }.ensuring {
    case waitingForHeader: WaitingForHeader =>
      waitingForHeader.untrustedState.targetLimit == untrustedState.targetLimit &&
        waitingForHeader.verifiedState.currentHeight() >= verifiedState.currentHeight() &&
        (waitingForHeader.verifiedState.currentHeight() > verifiedState.currentHeight() ||
          waitingForHeader.requestHeight < next)
    case _ => true
  }

}
