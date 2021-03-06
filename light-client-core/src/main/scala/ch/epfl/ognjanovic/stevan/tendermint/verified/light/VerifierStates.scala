package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.FetchedStacks.FetchedStack
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationTraces.VerificationTrace
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import stainless.annotation.inlineInvariant
import stainless.lang._

object VerifierStates {

  @inlineInvariant
  sealed abstract class VerifierState

  @inlineInvariant
  case class Finished(
    outcome: Either[Unit, VerificationError],
    verificationTrace: VerificationTrace,
    fetchedStack: FetchedStack)
      extends VerifierState {
    require(
      (outcome.isLeft && verificationTrace.currentHeight() == fetchedStack.targetLimit) ||
        (outcome.isRight && verificationTrace.currentHeight() < fetchedStack.targetLimit))
  }

  @inlineInvariant
  case class WaitingForHeader(requestHeight: Height, verificationTrace: VerificationTrace, fetchedStack: FetchedStack)
      extends VerifierState {
    require(
      fetchedStack.peek().map(requestHeight < _.header.height).getOrElse(true) &&
        verificationTrace.currentHeight() < requestHeight &&
        requestHeight <= fetchedStack.targetLimit)
  }

}
