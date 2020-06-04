package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.InvalidHeader
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.Failure
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import org.scalatest.flatspec.AnyFlatSpec

sealed class VerifierSequentialHeaderTests extends AnyFlatSpec {

  "Wrong chain id" should "fail verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/header/wrong_chain_id.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidHeader)
  }
}
