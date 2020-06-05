package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockValidators.LightBlockValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClientLemmas._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.UntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.Verifiers.Verifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.lang.StaticChecks.Ensuring
import stainless.lang._

case class MultiStepVerifier(
  lightBlockProvider: LightBlockProvider,
  lightBlockValidator: LightBlockValidator,
  verifier: Verifier,
  heightCalculator: NextHeightCalculator) {

  def verifyUntrusted(
    trustedState: TrustedState,
    untrustedState: UntrustedState): Either[Unit, VerificationError] = {
    require(
      trustedState.currentHeight() < untrustedState.targetLimit &&
        untrustedState.bottomHeight().isEmpty &&
        untrustedState.targetLimit <= lightBlockProvider.currentHeight)

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
    require(waitingForHeader.untrustedState.targetLimit <= lightBlockProvider.currentHeight)
    decreases(LightClientLemmas.terminationMeasure(waitingForHeader))

    processHeader(waitingForHeader, lightBlockProvider.lightBlock(waitingForHeader.requestHeight)) match {
      case state: WaitingForHeader => verify(state)
      case state: Finished => state
    }
  }

  private def processHeader(
    waitingForHeader: WaitingForHeader,
    lightBlock: LightBlock): VerifierState = {
    require(lightBlock.header.height == waitingForHeader.requestHeight)

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

    lightBlockValidator.validateUntrustedBlock(trustedState.trustedLightBlock, lightBlock) match {
      case error @ Right(_) =>
        val newUntrustedState = untrustedState.insertLightBlock(lightBlock)
        Finished(error, trustedState, newUntrustedState)

      case _: Left[Unit, VerificationError] =>
        verifier.verify(trustedState, lightBlock) match {
          case Success =>
            val newTrustedState = trustedState.increaseTrust(lightBlock)
            if (newTrustedState.currentHeight() == untrustedState.targetLimit) {
              val result = Left[Unit, VerificationError](())
              Finished(result, newTrustedState, untrustedState)
            } else if (untrustedState.hasNextHeader(newTrustedState.currentHeight(), untrustedState.targetLimit)) {
              val (nextLightBlock, nextUntrustedState) = untrustedState.removeBottom()
              stepByStepVerification(nextLightBlock, newTrustedState, nextUntrustedState)
            } else if (newTrustedState.currentHeight() + 1 == untrustedState.targetLimit)
              WaitingForHeader(untrustedState.targetLimit, newTrustedState, untrustedState)
            else {
              val newTargetHeight = heightCalculator.nextHeight(
                newTrustedState.currentHeight(),
                untrustedState.bottomHeight().getOrElse(untrustedState.targetLimit))
              WaitingForHeader(
                newTargetHeight,
                newTrustedState,
                untrustedState)
            }

          case InsufficientTrust =>
            val nextHeight = heightCalculator.nextHeight(trustedState.currentHeight(), lightBlock.header.height)
            val nextUntrustedState = untrustedState.insertLightBlock(lightBlock)

            WaitingForHeader(nextHeight, trustedState, nextUntrustedState)

          case Failure(reason) =>
            val error = Right[Unit, VerificationError](reason)
            val newUntrustedState = untrustedState.insertLightBlock(lightBlock)

            Finished(error, trustedState, newUntrustedState)
        }
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
