package ch.epfl.ognjanovic.stevan.tendermint

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.SignedHeader
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.{CirceDecoders, circe}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{LightBlock, ValidatorSet}
import io.circe.Decoder

package object light {

  implicit val singleStepTestCaseDecoder: Decoder[(LightBlock, Long, Instant, LightBlockProvider)] = cursor => for {
    signedHeader <- cursor.downField("initial")
      .downField("signed_header")
      .as[SignedHeader](CirceDecoders.signedHeaderDecoder)
    nextValidatorSet <- cursor.downField("initial")
      .downField("next_validator_set")
      .as[ValidatorSet](CirceDecoders.validatorSetDecoder)
    now <- cursor.downField("initial")
      .downField("now")
      .as[Instant](circe.instantDecoder)
    trustingPeriod <- cursor.downField("initial")
      .downField("trusting_period")
      .as[Long]
    provider <- cursor.downField("input")
      .as[LightBlockProvider](InMemoryProvider.decoder(LightClientIntegrationTests.lightBlockDecoder))
  } yield {
    (
      LightBlock(
        signedHeader.header,
        signedHeader.commit,
        nextValidatorSet,
        nextValidatorSet,
        LightClientIntegrationTests.defaultProvider),
      trustingPeriod,
      now,
      provider)
  }

}
