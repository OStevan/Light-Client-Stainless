package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.CommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockValidators.LightBlockValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.TrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.{Failure, InsufficientTrust, Success, VerificationOutcome}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.annotation.pure
import stainless.lang._

case class Verifier(validator: LightBlockValidator, trustVerifier: TrustVerifier, commitValidators: CommitValidator) {
  @pure
  def verify(trustedState: TrustedState, untrustedLightBlock: LightBlock): VerificationOutcome = {
    require(trustedState.currentHeight() < untrustedLightBlock.header.height)
    val validationResult = validator.validateUntrustedBlock(trustedState.trustedLightBlock, untrustedLightBlock)
    if (validationResult.isRight)
      Failure(validationResult.get)
    else {
      val verificationResult = trustVerifier.verify(trustedState, untrustedLightBlock)
      if (verificationResult != Success)
        verificationResult
      else
        commitValidators.isCommitInvalid(untrustedLightBlock) match {
          case Left(_) => Success
          case Right(content) => Failure(content)
        }
    }
  }.ensuring(res => ((res == Success) ==> trustedState.trusted(untrustedLightBlock)) &&
    ((res == InsufficientTrust) ==> (trustedState.currentHeight() + 1 < untrustedLightBlock.header.height)))
}
