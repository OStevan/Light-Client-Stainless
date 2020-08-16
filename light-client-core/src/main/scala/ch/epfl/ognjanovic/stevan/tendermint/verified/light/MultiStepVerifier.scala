package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.FetchedStacks.FetchedStack
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClientLemmas._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationTraces.VerificationTrace
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.lang._
import stainless.lang.StaticChecks.Ensuring

case class MultiStepVerifier(
  lightBlockProvider: LightBlockProvider,
  verifier: Verifier,
  heightCalculator: NextHeightCalculator) {

  def verifyUntrusted(verificationTrace: VerificationTrace, fetchedStack: FetchedStack): Finished = {
    require(
      verificationTrace.currentHeight() <= fetchedStack.targetLimit &&
        fetchedStack.peek().isEmpty &&
        fetchedStack.targetLimit <= lightBlockProvider.currentHeight)

    if (verificationTrace.currentHeight() == fetchedStack.targetLimit)
      Finished(Left(()), verificationTrace, fetchedStack)
    else {
      val nextHeight = fetchedStack.targetLimit
      verify(WaitingForHeader(nextHeight, verificationTrace, fetchedStack))
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

    stepByStepVerification(lightBlock, waitingForHeader.verificationTrace, waitingForHeader.fetchedStack)
  }.ensuring {
    case state: WaitingForHeader =>
      if (waitingForHeader.verificationTrace.currentHeight() == state.verificationTrace.currentHeight())
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
    verificationTrace: VerificationTrace,
    fetchedStack: FetchedStack): VerifierState = {
    require(
      lightBlock.header.height <= fetchedStack.targetLimit &&
        verificationTrace.currentHeight() < lightBlock.header.height &&
        fetchedStack.peek().map(lightBlock.header.height < _.header.height).getOrElse(true))
    decreases(fetchedStack.targetLimit.value - verificationTrace.currentHeight().value)

    verifier.verify(verificationTrace, lightBlock) match {
      case VerificationOutcomes.Success =>
        val newVerificationTrace = verificationTrace.increaseTrust(lightBlock)
        if (newVerificationTrace.currentHeight() == fetchedStack.targetLimit) {
          val result = Left[Unit, VerificationError](())
          Finished(result, newVerificationTrace, fetchedStack)
        } else if (fetchedStack.peek().isDefined) {
          val (nextLightBlock, nextFetchedStack) = fetchedStack.pop()
          stepByStepVerification(nextLightBlock, newVerificationTrace, nextFetchedStack)
        } else if (newVerificationTrace.currentHeight() + 1 == fetchedStack.targetLimit)
          WaitingForHeader(fetchedStack.targetLimit, newVerificationTrace, fetchedStack)
        else {
          val newTargetHeight = heightCalculator.nextHeight(
            newVerificationTrace.currentHeight(),
            fetchedStack.peek().map(_.header.height).getOrElse(fetchedStack.targetLimit))
          WaitingForHeader(newTargetHeight, newVerificationTrace, fetchedStack)
        }

      case VerificationOutcomes.InsufficientTrust =>
        val nextHeight = heightCalculator.nextHeight(verificationTrace.currentHeight(), lightBlock.header.height)
        val nextFetchedStack = fetchedStack.push(lightBlock)
        WaitingForHeader(nextHeight, verificationTrace, nextFetchedStack)

      case Failure(reason) =>
        val error = Right[Unit, VerificationError](reason)
        val newFetchedStack = fetchedStack.push(lightBlock)

        Finished(error, verificationTrace, newFetchedStack)
    }
  }.ensuring {
    case waitingForHeader: WaitingForHeader =>
      waitingForHeader.fetchedStack.targetLimit == fetchedStack.targetLimit &&
        waitingForHeader.verificationTrace.currentHeight() >= verificationTrace.currentHeight() &&
        (waitingForHeader.verificationTrace.currentHeight() > verificationTrace.currentHeight() ||
          waitingForHeader.requestHeight < lightBlock.header.height)
    case _ => true
  }

}
