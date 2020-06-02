package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.{Failure, InvalidCommit, Success}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import org.scalatest.flatspec.AnyFlatSpec

sealed class VerifierSequentialValidatorsTests extends AnyFlatSpec {

  "Single step sequential with one validator" should "succeed for height 2" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/1_validator.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Single step sequential with 8 validators" should "succeed for height 2" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/8_validators.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Single step sequential with 128 validators" should "succeed for height 2" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/128_validators.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "A missing validator in an adjacent block" should "fail with invalid commit" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/faulty_signer.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == InvalidCommit)
  }

  "Half of the validators changing between two adjacent blocks" should "not influence verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/half_valset_changes.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Less than one third of validators changing between two adjacent blocks" should "not influence verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/less_than_one_third_valset_changes.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "More than two thirds of validators changing between two adjacent blocks" should "not influence verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/more_than_two_thirds_valset_changes.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "One third of validators changing between two adjacent blocks" should "not influence verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/one_third_valset_changes.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Two thirds of validators changing between two adjacent blocks" should "not influence verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/two_thirds_valset_changes.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Complete change of validator sets for two adjacent blocks" should "not influence verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/valset_changes_fully.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Doubling of validator set size between two adjacent blocks" should "not influence verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/valset_size_doubles.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Halving of validator set size between two adjacent blocks" should "not influence verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/valset_size_halves.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Wrong validator sets" should "result in failed verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/validator_set/wrong_valset.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Failure)
  }
}
