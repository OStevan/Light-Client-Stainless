package ch.epfl.ognjanovic.stevan.tendermint

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.circe
import ch.epfl.ognjanovic.stevan.tendermint.rpc.types.SignedHeader
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.{SimpleTrustedState, TrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height, LightBlock, ValidatorSet}
import io.circe.Decoder
import io.circe.generic.semiauto._

package object light {

  implicit val lightBlockProviderDecoder: Decoder[LightBlockProvider] =
    InMemoryProvider.defaultChainDecoder(VerifierTests.lightBlockDecoder)

  case class TrustedInitialState(
    signed_header: SignedHeader,
    next_validator_set: ValidatorSet,
    now: Instant,
    trusting_period: Long)

  case class SingleStepTestCase(initial: TrustedInitialState, input: LightBlockProvider)

  implicit val trustedInitialStateDecoder: Decoder[TrustedInitialState] = deriveDecoder

  implicit val singleStepTestCase: Decoder[SingleStepTestCase] = deriveDecoder

  implicit val singleStepTestCaseDecoder
    : Decoder[(TrustedState, VotingPowerVerifier, Duration, Instant, LightBlockProvider)] =
    cursor =>
      for {
        testCase ← cursor.as[SingleStepTestCase]
      } yield {
        val trustVerifier = VotingPowerVerifiers.defaultTrustVerifier

        (
          SimpleTrustedState(
            LightBlock(
              testCase.initial.signed_header.header,
              testCase.initial.signed_header.commit,
              testCase.initial.next_validator_set,
              testCase.initial.next_validator_set,
              VerifierTests.defaultProvider
            ),
            trustVerifier
          ),
          trustVerifier,
          Duration(0, testCase.initial.trusting_period),
          testCase.initial.now,
          testCase.input)
      }

  implicit val multiStepTestCaseDecoder: Decoder[(TrustOptions, LightBlockProvider, Height, Instant)] = cursor =>
    for {
      trustOptions <- cursor.downField("trust_options").as[TrustOptions](TrustOptions.decoder)
      primary <-
        cursor
          .downField("primary")
          .as[LightBlockProvider](InMemoryProvider.decoder(VerifierTests.lightBlockDecoder))
      heightToVerify <- cursor.downField("height_to_verify").as[Long]
      now <- cursor.downField("now").as[Instant](circe.instantDecoder)
    } yield {
      (trustOptions, primary, Height(heightToVerify), now)
    }

}
