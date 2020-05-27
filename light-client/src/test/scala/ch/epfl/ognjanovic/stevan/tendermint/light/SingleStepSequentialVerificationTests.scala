package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.SequentialHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.{Failure, InvalidCommit, Success}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates.{Finished, WaitingForHeader}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{TrustVerifiers, TrustedState, UntrustedState, Verifier}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import org.scalatest.flatspec.AnyFlatSpec

sealed class SingleStepSequentialVerificationTests extends AnyFlatSpec {

  "Single step sequential with one validator" should "succeed for height 2" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/1_validator.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "Single step sequential with 8 validators" should "succeed for height 2" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/8_validators.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "Single step sequential with 128 validators" should "succeed for height 2" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/128_validators.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "A missing validator in an adjacent block" should "fail verification" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/faulty_signer.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "Half of the validators changing between two adjacent blocks" should "not influence verification" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/half_valset_changes.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "Less than one third of validators changing between two adjacent blocks" should "not influence verification" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/less_than_one_third_valset_changes.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "More than two thirds of validators changing between two adjacent blocks" should "not influence verification" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/more_than_two_thirds_valset_changes.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "One third of validators changing between two adjacent blocks" should "not influence verification" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/one_third_valset_changes.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "Two thirds of validators changing between two adjacent blocks" should "not influence verification" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/two_thirds_valset_changes.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "Complete change of validator sets for two adjacent blocks" should "not influence verification" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/valset_changes_fully.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "Doubling of validator set size between two adjacent blocks" should "not influence verification" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/valset_size_doubles.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "Halving of validator set size between two adjacent blocks" should "not influence verification" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/valset_size_halves.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

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

  "Wrong validator sets" should "result in failed verification" in {
    val content = LightClientIntegrationTests.content(
      "/single-step/sequential/validator_set/wrong_valset.json")
    val (trustedHeader, trustingPeriod, now, provider) =
      new CirceDeserializer(singleStepTestCaseDecoder)(content)

    val verifier = SingleStepSequentialVerificationTests.createVerifierWithDefaultTrustLevel(trustingPeriod, now)

    val requestHeight = Height(2)
    val result = verifier.processHeader(
      WaitingForHeader(
        requestHeight,
        requestHeight,
        TrustedState(trustedHeader, TrustVerifiers.defaultTrustVerifier),
        UntrustedState.empty),
      provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Finished])
    assert(result.asInstanceOf[Finished].outcome == Failure)
  }
}

object SingleStepSequentialVerificationTests {
  private def createVerifierWithDefaultTrustLevel(trustingPeriod: Long, now: Instant) = {
    val verifier = Verifier(
      new TimeBasedExpirationChecker(() => now, trustingPeriod),
      TrustVerifiers.defaultTrustVerifier,
      SequentialHeightCalculator)
    verifier
  }
}
