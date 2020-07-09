package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.UntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import stainless.annotation.inlineInvariant
import stainless.lang._

object VerifierStates {

  @inlineInvariant
  sealed abstract class VerifierState

  @inlineInvariant
  case class Finished(
    outcome: Either[Unit, VerificationError],
    trustedState: TrustedState,
    untrustedState: UntrustedState)
      extends VerifierState {
    require(
      (outcome.isLeft && trustedState.currentHeight() == untrustedState.targetLimit) ||
        (outcome.isRight && trustedState.currentHeight() < untrustedState.targetLimit))
  }

  @inlineInvariant
  case class WaitingForHeader(requestHeight: Height, trustedState: TrustedState, untrustedState: UntrustedState)
      extends VerifierState {
    require(
      untrustedState.bottomHeight().map(requestHeight < _).getOrElse(true) &&
        trustedState.currentHeight() < requestHeight &&
        requestHeight <= untrustedState.targetLimit)
  }

}
