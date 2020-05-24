package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.rpc.SignedHeader
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDecoders
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{LightBlock, PeerId, ValidatorSet}
import io.circe.Decoder

private object LightBlockDecoder {
  val decoder: PeerId => Decoder[LightBlock] =
    peerId =>
      cursor => for {
        signedHeader <- cursor.downField("signed_header").as[SignedHeader](CirceDecoders.signedHeaderDecoder)
        validatorSet <- cursor.downField("validator_set").as[ValidatorSet](CirceDecoders.validatorSetDecoder)
        nextValidatorSet <- cursor.downField("next_validator_set").as[ValidatorSet](CirceDecoders.validatorSetDecoder)
      } yield {
        LightBlock(signedHeader.header, signedHeader.commit, validatorSet, nextValidatorSet, peerId)
      }
}
