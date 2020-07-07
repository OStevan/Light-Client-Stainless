package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.light.cases.SingleStepTestCase
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.rpc.Deserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers
import ch.epfl.ognjanovic.stevan.tendermint.verified.types._
import org.scalatest.flatspec.AnyFlatSpec

sealed class VerifierBisectionVerificationTests extends AnyFlatSpec with VerifierTests {

  implicit private val testCaseDeserializer: Deserializer[SingleStepTestCase] =
    new CirceDeserializer(SingleStepTestCase.decoder)

  private val votingPowerVerifier = VotingPowerVerifiers.defaultVotingPowerVerifier

  "Verifying a block with the same validator sets" should "succeed" in {
    val (verifier, trustedState, provider) =
      buildTest(VerifierTests.testCase("/single-step/skipping/validator_set/skip_one_block.json"), votingPowerVerifier)

    val requestHeight = Height(3)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Verifying a block with the same validator sets" should "succeed for height 7" in {
    val (verifier, trustedState, provider) =
      buildTest(VerifierTests.testCase("/single-step/skipping/validator_set/skip_five_blocks.json"), votingPowerVerifier)

    val requestHeight = Height(7)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Verifying a block with sufficient overlap in validator sets" should "succeed for height 7" in {
    val (verifier, trustedState, provider) =
      buildTest(
        VerifierTests.testCase("/single-step/skipping/validator_set/valset_changes_less_than_trust_level.json"),
        votingPowerVerifier)

    val requestHeight = Height(7)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Verifying a block with insufficient overlap in validator sets" should "result in insufficient trust" in {
    val (verifier, trustedState, provider) =
      buildTest(
        VerifierTests.testCase("/single-step/skipping/validator_set/valset_changes_more_than_trust_level.json"),
        votingPowerVerifier)

    val requestHeight = Height(7)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == InsufficientTrust)
  }

  "Verifying a block with sufficient commit power" should "succeed for height 3" in {
    val (verifier, trustedState, provider) =
      buildTest(
        VerifierTests.testCase("/single-step/skipping/commit/more_than_two_third_vals_sign.json"),
        votingPowerVerifier)

    val requestHeight = Height(3)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Verifying a block with insufficient commit power" should "recognize an invalid commit for height 3" in {
    val (verifier, trustedState, provider) =
      buildTest(
        VerifierTests.testCase("/single-step/skipping/commit/one_third_vals_dont_sign.json"),
        votingPowerVerifier)

    val requestHeight = Height(3)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result.asInstanceOf[Failure].reason == InsufficientCommitPower)
  }

  "Verification with an expired trusted header" should "fail with expired trusted state" in {
    val (verifier, trustedState, provider) =
      buildTest(VerifierTests.testCase("/single-step/skipping/header/out_of_trusting_period.json"), votingPowerVerifier)

    val requestHeight = Height(6)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result.asInstanceOf[Failure].reason == ExpiredTrustedState)
  }
}
