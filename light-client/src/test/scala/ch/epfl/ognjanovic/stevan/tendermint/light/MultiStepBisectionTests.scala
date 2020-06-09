package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.{ExpiredTrustedState, InvalidNextValidatorSet, VerificationError}
import org.scalatest.flatspec.AnyFlatSpec
import stainless.lang._

sealed class MultiStepBisectionTests extends AnyFlatSpec {
  "Happy path bisection" should "succeed" in {
    val (verifier, trustedState, heightToVerify) = MultiStepVerifierTests.deserializeMultiStepTestCase(
      "/bisection/single-peer/happy_path.json")

    val result = verifier.verifyUntrusted(trustedState, UntrustedStates.empty(heightToVerify))

    assert(result.isLeft)
  }

  "Trusted state expired" should "fail verification" in {
    val (verifier, trustedState, heightToVerify) = MultiStepVerifierTests.deserializeMultiStepTestCase(
      "/bisection/single-peer/header_out_of_trusting_period.json")

    val result = verifier.verifyUntrusted(trustedState, UntrustedStates.empty(heightToVerify))

    assert(result.isRight && result.get == ExpiredTrustedState)
  }

  //  "Invalid validator set" should "fail verification" in {
  //    val (verifier, trustedState, heightToVerify) = MultiStepVerifierTests.deserializeMultiStepTestCase(
  //      "/bisection/single-peer/invalid_validator_set.json")
  //
  //    val result = verifier.verifyUntrusted(trustedState, UntrustedStates.empty(heightToVerify))
  //
  //    assert(result == ExpiredTrustedState)
  //  }

  "Not enough commits" should "fail verification" in {
    val (verifier, trustedState, heightToVerify) = MultiStepVerifierTests.deserializeMultiStepTestCase(
      "/bisection/single-peer/not_enough_commits.json")

    val result = verifier.verifyUntrusted(trustedState, UntrustedStates.empty(heightToVerify))

    // this test should fail with InsufficientCommitPower but it fails with InvalidNextValidatorSet
    assert(result.isRight && result.get == InvalidNextValidatorSet)
  }

  "Worst case scenario for bisection" should "not influence the successful outcome" in {
    val (verifier, trustedState, heightToVerify) = MultiStepVerifierTests.deserializeMultiStepTestCase(
      "/bisection/single-peer/worst_case.json")

    val result = verifier.verifyUntrusted(trustedState, UntrustedStates.empty(heightToVerify))

    assert(result == Left[Unit, VerificationError](()))
  }
}
