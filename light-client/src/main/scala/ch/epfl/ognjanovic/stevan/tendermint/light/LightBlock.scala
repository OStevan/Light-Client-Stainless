package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.rpc.types.{Address, Deserializer, SignedHeader, ValidatorSet}
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
  def deserializer(peer: Address): Deserializer[LightBlock] = new Deserializer[LightBlock] {
    override implicit val decoder: Decoder[LightBlock] = cursor => for {
      signedHeader <- cursor.downField("signed_header").as[SignedHeader](SignedHeader.deserializer.decoder)
      validatorSet <- cursor.downField("validator_set").as[ValidatorSet](ValidatorSet.deserializer.decoder)
      nextValidatorSet <- cursor.downField("next_validator_set").as[ValidatorSet](ValidatorSet.deserializer.decoder)
    } yield {
      LightBlock(signedHeader, validatorSet, nextValidatorSet, peer)
    }
  }
}
