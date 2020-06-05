package ch.epfl.ognjanovic.stevan.tendermint

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.SignedHeader
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.{CirceDecoders, circe}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.{SimpleTrustedState, TrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock, ValidatorSet}
import io.circe.Decoder

package object light {

  implicit val singleStepTestCaseDecoder: Decoder[(TrustedState, VotingPowerVerifier, Long, Instant, LightBlockProvider)] =
    cursor => for {
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
        .as[LightBlockProvider](InMemoryProvider.defaultChainDecoder(VerifierTests.lightBlockDecoder))
    } yield {
      val trustVerifier = VotingPowerVerifiers.defaultTrustVerifier

      (SimpleTrustedState(
        LightBlock(
          signedHeader.header,
          signedHeader.commit,
          nextValidatorSet,
          nextValidatorSet,
          VerifierTests.defaultProvider),
        trustVerifier),
        trustVerifier,
        trustingPeriod,
        now,
        provider)
    }

  implicit val multiStepTestCaseDecoder: Decoder[(TrustOptions, LightBlockProvider, Height, Instant)] = cursor => for {
    trustOptions <- cursor.downField("trust_options").as[TrustOptions](TrustOptions.decoder)
    primary <- cursor.downField("primary")
      .as[LightBlockProvider](InMemoryProvider.decoder(VerifierTests.lightBlockDecoder))
    heightToVerify <- cursor.downField("height_to_verify").as[Long]
    now <- cursor.downField("now").as[Instant](circe.instantDecoder)
  } yield {
    (trustOptions, primary, Height(heightToVerify), now)
  }

}
