package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.TrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.annotation.pure
import stainless.lang._

object Verifiers {

  abstract class Verifier {
    def verify(trustedState: TrustedState, untrustedLightBlock: LightBlock): VerificationOutcome
  }

  case class DefaultVerifier(expirationChecker: ExpirationChecker, trustVerifier: TrustVerifier) extends Verifier {
    @pure
    override def verify(trustedState: TrustedState, lightBlock: LightBlock): VerificationOutcome = {
      require(trustedState.currentHeight() < lightBlock.header.height)
      if (expirationChecker.isExpired(trustedState.trustedLightBlock))
        ExpiredTrustedState
      else if (trustedState.trusted(lightBlock))
        checkCommit(lightBlock)
      else if (trustedState.isAdjacent(lightBlock))
        Failure
      else
        InsufficientTrust
    }.ensuring(res => (res == Success) ==> trustedState.trusted(lightBlock))

    @pure
    def checkCommit(header: LightBlock): VerificationOutcome = {
      if (header.commit.forBlock.nonEmpty &&
        (header.commit.forBlock subsetOf header.validatorSet.keys) &&
        trustVerifier.consensusObtained(header.validatorSet, header.commit))
        Success
      else
        InvalidCommit
    }
  }

}
