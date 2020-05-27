package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.SequentialHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.Success
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
