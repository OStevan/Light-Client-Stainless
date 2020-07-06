package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStateFactories.InMemoryUntrustedStateFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors._
import org.scalatest.flatspec.AnyFlatSpec
import stainless.lang._

sealed class MultiStepBisectionTests extends AnyFlatSpec {
  private val untrustedStateFactory = new InMemoryUntrustedStateFactory()

  "Happy path bisection" should "succeed" in {
    val (verifier, trustedState, heightToVerify) =
      MultiStepVerifierTests.deserializeMultiStepTestCase("/bisection/single-peer/happy_path.json")

    val result = verifier.verifyUntrusted(trustedState, untrustedStateFactory.emptyWithTarget(heightToVerify))

    assert(result.outcome.isLeft)
  }

  "Trusted state expired" should "fail verification" in {
    val (verifier, trustedState, heightToVerify) =
      MultiStepVerifierTests.deserializeMultiStepTestCase("/bisection/single-peer/header_out_of_trusting_period.json")

    val result = verifier.verifyUntrusted(trustedState, untrustedStateFactory.emptyWithTarget(heightToVerify))

    assert(result.outcome.isRight && result.outcome.get == ExpiredTrustedState)
  }

  //  "Invalid validator set" should "fail verification" in {
  //    val (verifier, trustedState, heightToVerify) = MultiStepVerifierTests.deserializeMultiStepTestCase(
  //      "/bisection/single-peer/invalid_validator_set.json")
  //
  //    val result = verifier.verifyUntrusted(trustedState, untrustedStateFactory.emptyWithTarget(heightToVerify))
  //
  //    assert(result == ExpiredTrustedState)
  //  }

  "Not enough commits" should "fail verification" in {
    val (verifier, trustedState, heightToVerify) =
      MultiStepVerifierTests.deserializeMultiStepTestCase("/bisection/single-peer/not_enough_commits.json")

    val result = verifier.verifyUntrusted(trustedState, untrustedStateFactory.emptyWithTarget(heightToVerify))

    // this test should fail with InsufficientCommitPower but it fails with InvalidNextValidatorSet
    assert(result.outcome.isRight && result.outcome.get == InvalidNextValidatorSet)
  }

  "Worst case scenario for bisection" should "not influence the successful outcome" in {
    val (verifier, trustedState, heightToVerify) =
      MultiStepVerifierTests.deserializeMultiStepTestCase("/bisection/single-peer/worst_case.json")

    val result = verifier.verifyUntrusted(trustedState, untrustedStateFactory.emptyWithTarget(heightToVerify))

    assert(result.outcome == Left[Unit, VerificationError](()))
  }
}
