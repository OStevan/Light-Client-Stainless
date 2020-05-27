package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.SequentialHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.{InvalidCommit, Success}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates.{Finished, WaitingForHeader}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{TrustVerifiers, TrustedState, UntrustedState, Verifier}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import org.scalatest.flatspec.AnyFlatSpec

sealed class SingleStepSequentialCommitTests  extends AnyFlatSpec {
  "Less than one third of nil votes" should "succeed for height 2" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/commit/less_than_one_third_nil_votes.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialCommitTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

    val requestHeight = Height(2)
    val result = verifier.processHeader(
      WaitingForHeader(
        requestHeight,
        requestHeight,
        TrustedState(trustedHeader, TrustVerifiers.defaultTrustVerifier),
        UntrustedState.empty),
      provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Finished])
    assert(result.asInstanceOf[Finished].outcome == Success)
  }

  "More than two thirds of validators sign" should "succeed for height 2" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/commit/more_than_two_third_vals_sign.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialCommitTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

    val requestHeight = Height(2)
    val result = verifier.processHeader(
      WaitingForHeader(
        requestHeight,
        requestHeight,
        TrustedState(trustedHeader, TrustVerifiers.defaultTrustVerifier),
        UntrustedState.empty),
      provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Finished])
    assert(result.asInstanceOf[Finished].outcome == Success)
  }

  "Byzantine consensus not obtained on commit" should "succeed for height 2" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/commit/one_third_vals_don't_sign.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialCommitTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

    val requestHeight = Height(2)
    val result = verifier.processHeader(
      WaitingForHeader(
        requestHeight,
        requestHeight,
        TrustedState(trustedHeader, TrustVerifiers.defaultTrustVerifier),
        UntrustedState.empty),
      provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Finished])
    assert(result.asInstanceOf[Finished].outcome == InvalidCommit)
  }
}

object SingleStepSequentialCommitTests {
  private def createVerifierWithDefaultTrustLevel(trustingPeriod: Long, now: Instant) = {
    val verifier = Verifier(
      new TimeBasedExpirationChecker(() => now, trustingPeriod),
      TrustVerifiers.defaultTrustVerifier,
      SequentialHeightCalculator)
    verifier
  }
}
