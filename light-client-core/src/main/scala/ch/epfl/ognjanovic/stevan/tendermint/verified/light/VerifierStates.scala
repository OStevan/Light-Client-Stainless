package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.UntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import stainless.annotation.inlineInvariant

object VerifierStates {

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
    trustedState: TrustedState,
    untrustedState: UntrustedState) extends VerifierState {
    require(
      untrustedState.bottomHeight().forall(requestHeight < _) &&
        trustedState.currentHeight() < requestHeight &&
        requestHeight <= untrustedState.targetLimit)
  }

}
