package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.SignedHeader
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.{CirceDecoders, CirceDeserializer, circe}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightClient.{Finished, Success, VerifierStateMachine, WaitingForHeader}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{TrustedState, UntrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock, ValidatorSet}
import io.circe.Decoder
import org.scalatest.flatspec.AnyFlatSpec

sealed class SingleStepVerificationTests extends AnyFlatSpec {

  "Verifying a block with the same validator sets" should "succeed" in {
    val content = LightClientIntegrationTests.content(
      "/single-step-verification/validator-sets/same_validator_sets_1.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(SingleStepVerificationTests.singleStepTestCaseDecoder)(content)

    val verifier = VerifierStateMachine()

    val requestHeight = Height(3)
    val result = verifier.processHeader(
      WaitingForHeader(requestHeight, requestHeight, TrustedState(trustedHeader), UntrustedState.empty),
      provider.lightBlock(requestHeight))
    assert(result.isInstanceOf[Finished])
    assert(result.asInstanceOf[Finished].outcome == Success)
  }

}

object SingleStepVerificationTests {
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
