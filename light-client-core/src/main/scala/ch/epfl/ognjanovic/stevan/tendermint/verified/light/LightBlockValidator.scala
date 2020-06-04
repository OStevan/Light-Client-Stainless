package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.lang._

abstract class LightBlockValidator {

  def validateUntrustedBlock(
    trustedLightBlock: LightBlock,
    untrustedLightBlock: LightBlock): Either[Unit, VerificationError]

}
