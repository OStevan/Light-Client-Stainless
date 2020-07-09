package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.VerifiedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedTraces.UntrustedTrace
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
    verifiedState: VerifiedState,
    untrustedTrace: UntrustedTrace)
      extends VerifierState {
    require(
      (outcome.isLeft && verifiedState.currentHeight() == untrustedTrace.targetLimit) ||
        (outcome.isRight && verifiedState.currentHeight() < untrustedTrace.targetLimit))
  }

  @inlineInvariant
  case class WaitingForHeader(requestHeight: Height, verifiedState: VerifiedState, untrustedTrace: UntrustedTrace)
      extends VerifierState {
    require(
      untrustedTrace.bottomHeight().map(requestHeight < _).getOrElse(true) &&
        verifiedState.currentHeight() < requestHeight &&
        requestHeight <= untrustedTrace.targetLimit)
  }

}
