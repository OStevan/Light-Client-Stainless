package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.VerifiedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.InvalidNextValidatorSet
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.annotation.{opaque, pure}
import stainless.lang._

object TrustVerifiers {

  abstract class TrustVerifier {

    @pure
    def verify(verifiedState: VerifiedState, untrustedLightBlock: LightBlock): VerificationOutcome = {
      require(verifiedState.currentHeight() < untrustedLightBlock.header.height)
      ??? : VerificationOutcome
    }.ensuring(res =>
      ((res == Success) ==> verifiedState.isTrusted(untrustedLightBlock)) &&
        ((res == InsufficientTrust) ==> (verifiedState.currentHeight() + 1 < untrustedLightBlock.header.height)))

  }

  case class DefaultTrustVerifier() extends TrustVerifier {

    @pure @opaque
    override def verify(verifiedState: VerifiedState, untrustedLightBlock: LightBlock): VerificationOutcome = {
      require(verifiedState.currentHeight() < untrustedLightBlock.header.height)
      if (verifiedState.isTrusted(untrustedLightBlock))
        Success
      else if (verifiedState.isAdjacent(untrustedLightBlock))
        Failure(InvalidNextValidatorSet)
      else
        InsufficientTrust
    }.ensuring(res =>
      ((res == Success) ==> verifiedState.isTrusted(untrustedLightBlock)) &&
        ((res == InsufficientTrust) ==> (verifiedState.currentHeight() + 1 < untrustedLightBlock.header.height)))

  }

}
