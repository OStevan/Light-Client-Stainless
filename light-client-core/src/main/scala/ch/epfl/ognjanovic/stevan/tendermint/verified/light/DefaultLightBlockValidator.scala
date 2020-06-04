package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.CommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.lang._

case class DefaultLightBlockValidator(
  expirationChecker: ExpirationChecker,
  commitValidator: CommitValidator) extends LightBlockValidator {
  override def validateUntrustedBlock(
    trustedLightBlock: LightBlock,
    untrustedLightBlock: LightBlock): Either[Unit, VerificationErrors.VerificationError] = {
    if (expirationChecker.isExpired(trustedLightBlock))
      Right(ExpiredTrustedState)
    else if (!untrustedLightBlock.header.time.isAfter(trustedLightBlock.header.time))
      Right(InvalidHeader)
    else if (trustedLightBlock.header.chainId != untrustedLightBlock.header.chainId)
      Right(InvalidHeader)
    else
      commitValidator.isCommitInvalid(untrustedLightBlock)
  }
}
