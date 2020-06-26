package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.annotation.pure
import stainless.lang._

object LightBlockValidators {

  abstract class LightBlockValidator {

    @pure
    def validateUntrustedBlock(
      trustedLightBlock: LightBlock,
      untrustedLightBlock: LightBlock): Either[Unit, VerificationError]

  }

  case class DummyLightBlockValidator() extends LightBlockValidator {

    @pure
    override def validateUntrustedBlock(
      trustedLightBlock: LightBlock,
      untrustedLightBlock: LightBlock): Either[Unit, VerificationError] = Left(())

  }

}
