package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClientLemmas._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.UntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.Verifiers.Verifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types._
import stainless.lang.StaticChecks.Ensuring
import stainless.lang._

case class LightClient(
  verifier: Verifier,
  heightCalculator: NextHeightCalculator) {


  def processHeader(
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
        untrustedState.bottomHeight().forall(lightBlock.header.height < _))
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
        else
          WaitingForHeader(
            heightCalculator.nextHeight(
              newTrustedState.currentHeight(),
              untrustedState.bottomHeight().getOrElse(untrustedState.targetLimit)),
            newTrustedState,
            untrustedState)

      case InsufficientTrust =>
        WaitingForHeader(
          heightCalculator.nextHeight(trustedState.currentHeight(), lightBlock.header.height),
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
      waitingForHeader.untrustedState.targetLimit == untrustedState.targetLimit &&
        waitingForHeader.trustedState.currentHeight() >= trustedState.currentHeight() &&
        (waitingForHeader.trustedState.currentHeight() > trustedState.currentHeight() ||
          waitingForHeader.requestHeight < lightBlock.header.height)
    case _ => true
  }
}
