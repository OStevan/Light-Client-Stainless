package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.InvalidNextValidatorSet
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationTraces.VerificationTrace
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.annotation.{opaque, pure}
import stainless.lang._

object TrustVerifiers {

  abstract class TrustVerifier {

    @pure
    def verify(verificationTrace: VerificationTrace, untrustedLightBlock: LightBlock): VerificationOutcome = {
      require(verificationTrace.currentHeight() < untrustedLightBlock.header.height)
      ??? : VerificationOutcome
    }.ensuring(res =>
      ((res == Success) ==> verificationTrace.isTrusted(untrustedLightBlock)) &&
        ((res == InsufficientTrust) ==> (verificationTrace.currentHeight() + 1 < untrustedLightBlock.header.height)))

  }

  case class DefaultTrustVerifier() extends TrustVerifier {

    @pure @opaque
    override def verify(verificationTrace: VerificationTrace, untrustedLightBlock: LightBlock): VerificationOutcome = {
      require(verificationTrace.currentHeight() < untrustedLightBlock.header.height)
      if (verificationTrace.isTrusted(untrustedLightBlock))
        Success
      else if (verificationTrace.isAdjacent(untrustedLightBlock))
        Failure(InvalidNextValidatorSet)
      else
        InsufficientTrust
    }.ensuring(res =>
      ((res == Success) ==> verificationTrace.isTrusted(untrustedLightBlock)) &&
        ((res == InsufficientTrust) ==> (verificationTrace.currentHeight() + 1 < untrustedLightBlock.header.height)))

  }

}
