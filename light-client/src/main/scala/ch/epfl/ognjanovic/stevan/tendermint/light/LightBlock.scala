package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.rpc.SignedHeader
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Address, ValidatorSet}
import io.circe.Decoder

/**
 * Data structure which holds all the necessary data for light client core verification as specified in the english spec.
 */
case class LightBlock(
  signedHeader: SignedHeader,
  validatorSet: ValidatorSet,
  nextValidatorSet: ValidatorSet,
  peer: Address)

object LightBlock {

  import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDecoders._

  def decoder(peer: Address): Decoder[LightBlock] = cursor => for {
    signedHeader <- cursor.downField("signed_header").as[SignedHeader]
    validatorSet <- cursor.downField("validator_set").as[ValidatorSet]
    nextValidatorSet <- cursor.downField("next_validator_set").as[ValidatorSet]
  } yield {
    LightBlock(signedHeader, validatorSet, nextValidatorSet, peer)
  }
}
