package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClientLemmas._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.UntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.Verifiers.Verifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.lang.StaticChecks.Ensuring
import stainless.lang._

case class MultiStepVerifier(
  lightBlockProvider: LightBlockProvider,
  verifier: Verifier,
  heightCalculator: NextHeightCalculator) {

  def verifyUntrusted(
    trustedState: TrustedState,
    untrustedState: UntrustedState): VerificationOutcome = {
    require(
      trustedState.currentHeight() < untrustedState.targetLimit &&
        untrustedState.bottomHeight().isEmpty &&
        untrustedState.targetLimit < lightBlockProvider.currentHeight)

    val nextHeight = if (trustedState.currentHeight() + 1 == untrustedState.targetLimit)
      untrustedState.targetLimit
    else
      heightCalculator.nextHeight(trustedState.currentHeight(), untrustedState.targetLimit)

    assert(untrustedState.bottomHeight().map(nextHeight < _).getOrElse(true))
    assert(trustedState.currentHeight() < nextHeight)
    assert(nextHeight <= untrustedState.targetLimit)
    verify(WaitingForHeader(nextHeight, trustedState, untrustedState)).outcome
  }

  @scala.annotation.tailrec
  private def verify(
    waitingForHeader: WaitingForHeader): Finished = {
    require(waitingForHeader.untrustedState.targetLimit < lightBlockProvider.currentHeight)
    decreases(LightClientLemmas.terminationMeasure(waitingForHeader))

    assert(waitingForHeader.requestHeight < lightBlockProvider.currentHeight)
    processHeader(waitingForHeader, lightBlockProvider.lightBlock(waitingForHeader.requestHeight)) match {
      case state: WaitingForHeader => verify(state)
      case state: Finished => state
    }
  }

  private def processHeader(
    waitingForHeader: WaitingForHeader,
    lightBlock: LightBlock): VerifierState = {
    require(lightBlock.header.height == waitingForHeader.requestHeight)
    assert(lightBlock.header.height <= waitingForHeader.untrustedState.targetLimit)
    assert(waitingForHeader.trustedState.currentHeight() < lightBlock.header.height)
    assert(waitingForHeader.untrustedState.bottomHeight().map(lightBlock.header.height < _).getOrElse(true))
    stepByStepVerification(
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
        state.untrustedState.targetLimit == waitingForHeader.untrustedState.targetLimit

    case _: Finished => true
  }

  private def stepByStepVerification(
    lightBlock: LightBlock,
    trustedState: TrustedState,
    untrustedState: UntrustedState): VerifierState = {
    require(
      lightBlock.header.height <= untrustedState.targetLimit &&
        trustedState.currentHeight() < lightBlock.header.height &&
        untrustedState.bottomHeight().map(lightBlock.header.height < _).getOrElse(true))
    decreases(untrustedState.targetLimit.value - trustedState.currentHeight().value)
    verifier.verify(trustedState, lightBlock) match {
      case Success =>
        val newTrustedState = trustedState.increaseTrust(lightBlock)
        if (newTrustedState.currentHeight() == untrustedState.targetLimit)
          Finished(Success, newTrustedState, untrustedState)
        else if (untrustedState.hasNextHeader(newTrustedState.currentHeight(), untrustedState.targetLimit)) {
          val (nextLightBlock, nextUntrustedState) = untrustedState.removeBottom()
          stepByStepVerification(nextLightBlock, newTrustedState, nextUntrustedState)
        } else if (newTrustedState.currentHeight() + 1 == untrustedState.targetLimit)
          WaitingForHeader(untrustedState.targetLimit, newTrustedState, untrustedState)
        else {
          val newTargetHeight = heightCalculator.nextHeight(
            newTrustedState.currentHeight(),
            untrustedState.bottomHeight().getOrElse(untrustedState.targetLimit))

          assert(untrustedState.bottomHeight().map(newTargetHeight < _).getOrElse(true))
          assert(trustedState.currentHeight() < newTargetHeight)
          assert(newTargetHeight <= untrustedState.targetLimit)
          WaitingForHeader(
            newTargetHeight,
            newTrustedState,
            untrustedState)
        }

      case InsufficientTrust =>
        val nextHeight = heightCalculator.nextHeight(trustedState.currentHeight(), lightBlock.header.height)
        val nextUntrustedState = untrustedState.insertLightBlock(lightBlock)
        assert(nextUntrustedState.bottomHeight().map(nextHeight < _).getOrElse(true))
        assert(trustedState.currentHeight() < nextHeight)
        assert(nextHeight <= nextUntrustedState.targetLimit)
        WaitingForHeader(nextHeight, trustedState, nextUntrustedState)

      case ExpiredTrustedState =>
        Finished(ExpiredTrustedState, trustedState, untrustedState.insertLightBlock(lightBlock))
      case InvalidCommit =>
        Finished(InvalidCommit, trustedState, untrustedState.insertLightBlock(lightBlock))
      case Failure =>
        Finished(Failure, trustedState, untrustedState.insertLightBlock(lightBlock))
    }
  }.ensuring {
    case waitingForHeader: WaitingForHeader =>
      waitingForHeader.untrustedState.targetLimit == untrustedState.targetLimit &&
        waitingForHeader.trustedState.currentHeight() >= trustedState.currentHeight() &&
        (waitingForHeader.trustedState.currentHeight() > trustedState.currentHeight() ||
          waitingForHeader.requestHeight < lightBlock.header.height)
    case _ => true
  }
}
