package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.Success
import org.scalatest.flatspec.AnyFlatSpec

sealed class MultiStepBisectionTests extends AnyFlatSpec {
  "Happy path bisection" should "succeed" in {
    val (verifier, trustedState, heightToVerify) = MultiStepVerifierTests.deserializeMultiStepTestCase(
      "/bisection/single-peer/happy_path.json")

    val result = verifier.verifyUntrusted(trustedState, UntrustedStates.empty(heightToVerify))

    assert(result == Success)
  }
}
