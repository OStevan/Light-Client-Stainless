package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.FetchedStacks.FetchedStack
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.VerifiedState
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
    untrustedState: FetchedStack)
      extends VerifierState {
    require(
      (outcome.isLeft && verifiedState.currentHeight() == untrustedState.targetLimit) ||
        (outcome.isRight && verifiedState.currentHeight() < untrustedState.targetLimit))
  }

  @inlineInvariant
  case class WaitingForHeader(requestHeight: Height, verifiedState: VerifiedState, untrustedState: FetchedStack)
      extends VerifierState {
    require(
      untrustedState.peek().map(requestHeight < _.header.height).getOrElse(true) &&
        verifiedState.currentHeight() < requestHeight &&
        requestHeight <= untrustedState.targetLimit)
  }

}
