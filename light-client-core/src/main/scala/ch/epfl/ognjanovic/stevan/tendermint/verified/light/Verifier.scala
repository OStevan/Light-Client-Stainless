package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.CommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockValidators.LightBlockValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.TrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.{
  Failure,
  InsufficientTrust,
  Success,
  VerificationOutcome
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationTraces.VerificationTrace
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.annotation.pure
import stainless.lang._

case class Verifier(validator: LightBlockValidator, trustVerifier: TrustVerifier, commitValidators: CommitValidator) {

  @pure
  def verify(verificationTrace: VerificationTrace, untrustedLightBlock: LightBlock): VerificationOutcome = {
    require(verificationTrace.currentHeight() < untrustedLightBlock.header.height)
    val validationResult = validator.validateUntrustedBlock(verificationTrace.verified, untrustedLightBlock)
    if (validationResult.isRight)
      Failure(validationResult.get)
    else {
      val verificationResult = trustVerifier.verify(verificationTrace, untrustedLightBlock)
      if (verificationResult != Success)
        verificationResult
      else
        commitValidators.hasSufficientSignersOverlap(untrustedLightBlock) match {
          case Left(_) => Success
          case Right(content) => Failure(content)
        }
    }
  }.ensuring(res =>
    ((res == Success) ==> verificationTrace.isTrusted(untrustedLightBlock)) &&
      ((res == InsufficientTrust) ==> (verificationTrace.currentHeight() + 1 < untrustedLightBlock.header.height)))

}
