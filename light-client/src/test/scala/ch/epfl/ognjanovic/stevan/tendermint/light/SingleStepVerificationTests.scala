package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.SignedHeader
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.{CirceDecoders, CirceDeserializer, circe}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types._
import io.circe.Decoder
import org.scalatest.flatspec.AnyFlatSpec

sealed class SingleStepVerificationTests extends AnyFlatSpec {

  "Verifying a block with the same validator sets" should "succeed" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/skipping/validator_set/skip_one_block.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(SingleStepVerificationTests.singleStepTestCaseDecoder)(content)

    val verifier = Verifier(
      new TimeBasedExpirationChecker(() => now, trustingPeriod), DefaultTrustVerifier)

    val requestHeight = Height(3)
    val result = verifier.processHeader(
      WaitingForHeader(
        requestHeight,
        requestHeight,
        TrustedState(trustedHeader, DefaultTrustVerifier),
        UntrustedState.empty),
      provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Finished])
    assert(result.asInstanceOf[Finished].outcome == Success)
  }

  "Verifying a block with the same validator sets" should "succeed for height 7" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/skipping/validator_set/skip_five_blocks.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(SingleStepVerificationTests.singleStepTestCaseDecoder)(content)

    val verifier = Verifier(
      new TimeBasedExpirationChecker(() => now, trustingPeriod), DefaultTrustVerifier)

    val requestHeight = Height(7)
    val result = verifier.processHeader(
      WaitingForHeader(
        requestHeight,
        requestHeight,
        TrustedState(trustedHeader, DefaultTrustVerifier),
        UntrustedState.empty),
      provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Finished])
    assert(result.asInstanceOf[Finished].outcome == Success)
  }

  "Verifying a block with sufficient overlap in validator sets" should "succeed for height 7" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/skipping/validator_set/valset_changes_less_than_trust_level.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(SingleStepVerificationTests.singleStepTestCaseDecoder)(content)

    val verifier = Verifier(
      new TimeBasedExpirationChecker(() => now, trustingPeriod), DefaultTrustVerifier)

    val requestHeight = Height(7)
    val result = verifier.processHeader(
      WaitingForHeader(
        requestHeight,
        requestHeight,
        TrustedState(trustedHeader, DefaultTrustVerifier),
        UntrustedState.empty),
      provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Finished])
    assert(result.asInstanceOf[Finished].outcome == Success)
  }

  "Verifying a block with insufficient overlap in validator sets" should "request an intermediate for height 7" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/skipping/validator_set/valset_changes_more_than_trust_level.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(SingleStepVerificationTests.singleStepTestCaseDecoder)(content)

    val verifier = Verifier(
      new TimeBasedExpirationChecker(() => now, trustingPeriod), DefaultTrustVerifier)

    val requestHeight = Height(7)
    val result = verifier.processHeader(
      WaitingForHeader(
        requestHeight,
        requestHeight,
        TrustedState(trustedHeader, DefaultTrustVerifier),
        UntrustedState.empty),
      provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[WaitingForHeader])
    assert(result.asInstanceOf[WaitingForHeader].requestHeight == Height(4))
    assert(result.asInstanceOf[WaitingForHeader].targetHeight == Height(7))
  }

  "Verifying a block with sufficient commit power" should "succeed for height 3" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/skipping/commit/more_than_two_third_vals_sign.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(SingleStepVerificationTests.singleStepTestCaseDecoder)(content)

    val verifier = Verifier(
      new TimeBasedExpirationChecker(() => now, trustingPeriod), DefaultTrustVerifier)

    val requestHeight = Height(3)
    val result = verifier.processHeader(
      WaitingForHeader(
        requestHeight,
        requestHeight,
        TrustedState(trustedHeader, DefaultTrustVerifier),
        UntrustedState.empty),
      provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Finished])
    assert(result.asInstanceOf[Finished].outcome == Success)
  }

  "Verifying a block with insufficient commit power" should "recognize an invalid commit for height 3" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/skipping/commit/one_third_vals_dont_sign.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(SingleStepVerificationTests.singleStepTestCaseDecoder)(content)

    val verifier = Verifier(
      new TimeBasedExpirationChecker(() => now, trustingPeriod), DefaultTrustVerifier)

    val requestHeight = Height(3)
    val result = verifier.processHeader(
      WaitingForHeader(
        requestHeight,
        requestHeight,
        TrustedState(trustedHeader, DefaultTrustVerifier),
        UntrustedState.empty),
      provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Finished])
    assert(result.asInstanceOf[Finished].outcome == InvalidCommit)
  }

  "Verification with an expired trusted header" should "fail" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/skipping/header/out_of_trusting_period.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(SingleStepVerificationTests.singleStepTestCaseDecoder)(content)

    val verifier = Verifier(
      new TimeBasedExpirationChecker(() => now, trustingPeriod), DefaultTrustVerifier)

    val requestHeight = Height(5)
    val result = verifier.processHeader(
      WaitingForHeader(
        requestHeight,
        requestHeight,
        TrustedState(trustedHeader, DefaultTrustVerifier),
        UntrustedState.empty),
      provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Finished])
    assert(result.asInstanceOf[Finished].outcome != Success)
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
