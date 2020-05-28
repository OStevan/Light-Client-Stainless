package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.UntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.inlineInvariant
import stainless.collection._

object VerifierStates {

  @inline
  def targetHeightInvariant(targetHeight: Height, untrustedState: List[LightBlock]): Boolean = {
    untrustedState.forall(_.header.height <= targetHeight)
  }

  sealed abstract class VerifierState

  @inlineInvariant
  case class Finished(
    outcome: VerificationOutcome,
    trustedState: TrustedState,
    untrustedState: UntrustedState) extends VerifierState {
    require(
      (outcome == Success && trustedState.currentHeight() == untrustedState.targetLimit) ||
        (outcome != Success && trustedState.currentHeight() < untrustedState.targetLimit))
  }

  @inlineInvariant
  case class WaitingForHeader(
    requestHeight: Height,
    targetHeight: Height,
    trustedState: TrustedState,
    untrustedState: UntrustedState) extends VerifierState {
    require(
      requestHeight <= targetHeight &&
        trustedState.currentHeight() < requestHeight &&
        untrustedState.bottomHeight().map(requestHeight < _).getOrElse(true) &&
        untrustedState.targetLimit == targetHeight)
  }

}
