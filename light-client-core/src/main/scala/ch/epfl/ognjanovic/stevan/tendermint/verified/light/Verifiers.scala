package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.TrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.annotation.pure
import stainless.lang._

object Verifiers {

  abstract class Verifier {
    @pure
    def verify(trustedState: TrustedState, untrustedLightBlock: LightBlock): VerificationOutcome = {
      require(trustedState.currentHeight() < untrustedLightBlock.header.height)
      ??? : VerificationOutcome
    }.ensuring(res => ((res == Success) ==> trustedState.trusted(untrustedLightBlock)) &&
      ((res == InsufficientTrust) ==> (trustedState.currentHeight() + 1  < untrustedLightBlock.header.height)))
  }

  case class DefaultVerifier(expirationChecker: ExpirationChecker, trustVerifier: TrustVerifier) extends Verifier {
    @pure
    override def verify(trustedState: TrustedState, untrustedLightBlock: LightBlock): VerificationOutcome = {
      require(trustedState.currentHeight() < untrustedLightBlock.header.height)
      if (expirationChecker.isExpired(trustedState.trustedLightBlock))
        ExpiredTrustedState
      else if (trustedState.trusted(untrustedLightBlock))
        checkCommit(untrustedLightBlock)
      else if (trustedState.isAdjacent(untrustedLightBlock))
        Failure
      else
        InsufficientTrust
    }.ensuring(res => ((res == Success) ==> trustedState.trusted(untrustedLightBlock)) &&
      ((res == InsufficientTrust) ==> (trustedState.currentHeight() + 1  < untrustedLightBlock.header.height)))

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
