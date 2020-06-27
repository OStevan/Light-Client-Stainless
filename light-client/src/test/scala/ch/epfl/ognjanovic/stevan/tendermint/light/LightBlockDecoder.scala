package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDecoders
import ch.epfl.ognjanovic.stevan.tendermint.rpc.types.SignedHeader
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{LightBlock, PeerId, ValidatorSet}
import io.circe.Decoder

private object LightBlockDecoder {

  val decoder: PeerId => Decoder[LightBlock] =
    peerId =>
      cursor =>
        for {
          signedHeader <- cursor.downField("signed_header").as[SignedHeader](CirceDecoders.signedHeaderDecoder)
          validatorSet <-
            cursor.downField("validator_set").as[ValidatorSet](CirceDecoders.conformanceTestValidatorSetDecoder)
          nextValidatorSet <-
            cursor.downField("next_validator_set").as[ValidatorSet](CirceDecoders.conformanceTestValidatorSetDecoder)
        } yield {
          LightBlock(signedHeader.header, signedHeader.commit, validatorSet, nextValidatorSet, peerId)
        }

}
