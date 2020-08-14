package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.FetchedStacks.FetchedStack
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

  def verifyUntrusted(verifiedState: VerifiedState, fetchedStack: FetchedStack): Finished = {
    require(
      verifiedState.currentHeight() <= fetchedStack.targetLimit &&
        fetchedStack.peek().isEmpty &&
        fetchedStack.targetLimit <= lightBlockProvider.currentHeight)

    if (verifiedState.currentHeight() == fetchedStack.targetLimit)
      Finished(Left(()), verifiedState, fetchedStack)
    else {
      val nextHeight = fetchedStack.targetLimit
      verify(WaitingForHeader(nextHeight, verifiedState, fetchedStack))
    }
  }

  @scala.annotation.tailrec
  private def verify(waitingForHeader: WaitingForHeader): Finished = {
    require(waitingForHeader.fetchedStack.targetLimit <= lightBlockProvider.currentHeight)
    decreases(LightClientLemmas.terminationMeasure(waitingForHeader))

    processHeader(waitingForHeader, lightBlockProvider.lightBlock(waitingForHeader.requestHeight)) match {
      case state: WaitingForHeader => verify(state)
      case state: Finished => state
    }
  }

  private def processHeader(waitingForHeader: WaitingForHeader, lightBlock: LightBlock): VerifierState = {
    require(lightBlock.header.height == waitingForHeader.requestHeight)

    stepByStepVerification(lightBlock, waitingForHeader.verifiedState, waitingForHeader.fetchedStack)
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
      state.fetchedStack.targetLimit == waitingForHeader.fetchedStack.targetLimit

    case _: Finished => true
  }

  private def stepByStepVerification(
    lightBlock: LightBlock,
    verifiedState: VerifiedState,
    fetchedStack: FetchedStack): VerifierState = {
    require(
      lightBlock.header.height <= fetchedStack.targetLimit &&
        verifiedState.currentHeight() < lightBlock.header.height &&
        fetchedStack.peek().map(lightBlock.header.height < _.header.height).getOrElse(true))
    decreases(fetchedStack.targetLimit.value - verifiedState.currentHeight().value)

    verifier.verify(verifiedState, lightBlock) match {
      case VerificationOutcomes.Success =>
        val newVerifiedState = verifiedState.increaseTrust(lightBlock)
        if (newVerifiedState.currentHeight() == fetchedStack.targetLimit) {
          val result = Left[Unit, VerificationError](())
          Finished(result, newVerifiedState, fetchedStack)
        } else if (fetchedStack.peek().isDefined) {
          val (nextLightBlock, nextFetchedStack) = fetchedStack.pop()
          stepByStepVerification(nextLightBlock, newVerifiedState, nextFetchedStack)
        } else if (newVerifiedState.currentHeight() + 1 == fetchedStack.targetLimit)
          WaitingForHeader(fetchedStack.targetLimit, newVerifiedState, fetchedStack)
        else {
          val newTargetHeight = heightCalculator.nextHeight(
            newVerifiedState.currentHeight(),
            fetchedStack.peek().map(_.header.height).getOrElse(fetchedStack.targetLimit))
          WaitingForHeader(newTargetHeight, newVerifiedState, fetchedStack)
        }

      case VerificationOutcomes.InsufficientTrust =>
        val nextHeight = heightCalculator.nextHeight(verifiedState.currentHeight(), lightBlock.header.height)
        val nextFetchedStack = fetchedStack.push(lightBlock)
        WaitingForHeader(nextHeight, verifiedState, nextFetchedStack)

      case Failure(reason) =>
        val error = Right[Unit, VerificationError](reason)
        val newFetchedStack = fetchedStack.push(lightBlock)

        Finished(error, verifiedState, newFetchedStack)
    }
  }.ensuring {
    case waitingForHeader: WaitingForHeader =>
      waitingForHeader.fetchedStack.targetLimit == fetchedStack.targetLimit &&
        waitingForHeader.verifiedState.currentHeight() >= verifiedState.currentHeight() &&
        (waitingForHeader.verifiedState.currentHeight() > verifiedState.currentHeight() ||
          waitingForHeader.requestHeight < lightBlock.header.height)
    case _ => true
  }

}
