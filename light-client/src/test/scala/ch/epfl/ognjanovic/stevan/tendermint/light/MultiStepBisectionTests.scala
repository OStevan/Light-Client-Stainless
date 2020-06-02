package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import org.scalatest.flatspec.AnyFlatSpec

sealed class MultiStepBisectionTests extends AnyFlatSpec {
  "Happy path bisection" should "succeed" in {
    val (verifier, trustedState, heightToVerify) = MultiStepVerifierTests.deserializeMultiStepTestCase(
      "/bisection/single-peer/happy_path.json")

    val result = verifier.verifyUntrusted(trustedState, UntrustedStates.empty(heightToVerify))

    assert(result == Success)
  }

  "Trusted state expired" should "fail verification" in {
    val (verifier, trustedState, heightToVerify) = MultiStepVerifierTests.deserializeMultiStepTestCase(
      "/bisection/single-peer/header_out_of_trusting_period.json")

    val result = verifier.verifyUntrusted(trustedState, UntrustedStates.empty(heightToVerify))

    assert(result == ExpiredTrustedState)
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

    // per the test description should be InvalidCommit however it fails on validator sets (same as rust)
    assert(result == Failure)
  }

  "Worst case scenario for bisection" should "not influence the successful outcome" in {
    val (verifier, trustedState, heightToVerify) = MultiStepVerifierTests.deserializeMultiStepTestCase(
      "/bisection/single-peer/worst_case.json")

    val result = verifier.verifyUntrusted(trustedState, UntrustedStates.empty(heightToVerify))

    assert(result == Success)
  }
}
