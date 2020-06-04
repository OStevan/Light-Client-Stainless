package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.TrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.InvalidNextValidatorSet
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.annotation.{opaque, pure}
import stainless.lang._

object Verifiers {

  abstract class Verifier {
    @pure
    def verify(trustedState: TrustedState, untrustedLightBlock: LightBlock): VerificationOutcome = {
      require(trustedState.currentHeight() < untrustedLightBlock.header.height)
      ??? : VerificationOutcome
    }.ensuring(res => ((res == Success) ==> trustedState.trusted(untrustedLightBlock)) &&
      ((res == InsufficientTrust) ==> (trustedState.currentHeight() + 1 < untrustedLightBlock.header.height)))
  }

  case class DefaultVerifier(expirationChecker: ExpirationChecker, trustVerifier: TrustVerifier) extends Verifier {
    @pure @opaque
    override def verify(trustedState: TrustedState, untrustedLightBlock: LightBlock): VerificationOutcome = {
      require(trustedState.currentHeight() < untrustedLightBlock.header.height)
      if (trustedState.trusted(untrustedLightBlock))
        Success
      else if (trustedState.isAdjacent(untrustedLightBlock))
        Failure(InvalidNextValidatorSet)
      else
        InsufficientTrust
    }.ensuring(res => ((res == Success) ==> trustedState.trusted(untrustedLightBlock)) &&
      ((res == InsufficientTrust) ==> (trustedState.currentHeight() + 1 < untrustedLightBlock.header.height)))
  }

}
