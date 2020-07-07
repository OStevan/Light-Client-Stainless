package ch.epfl.ognjanovic.stevan.tendermint.light.cases

import ch.epfl.ognjanovic.stevan.tendermint.rpc.types.SignedHeader
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.ValidatorSet
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDecoders.{
  conformanceTestValidatorSetDecoder,
  signedHeaderDecoder
}
import io.circe.Decoder
import io.circe.generic.semiauto._

case class InputFormat(signed_header: SignedHeader, validator_set: ValidatorSet, next_validator_set: ValidatorSet)

object InputFormat {
  implicit val inputFormatDecoder: Decoder[InputFormat] = deriveDecoder
}
