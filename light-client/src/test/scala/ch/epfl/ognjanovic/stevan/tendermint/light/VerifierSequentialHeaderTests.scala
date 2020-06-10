package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.{InvalidCommitValue, InvalidHeader, InvalidNextValidatorSetHash, InvalidValidatorSetHash}
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

  "Wrong header/commit height pair" should "fail verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/header/wrong_header_height.json")

    val requestHeight = Height(3)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    // fails with invalid commit value as does the rust implementation because of hashes instead of height
    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidCommitValue)
  }

  "Wrong header timestamp" should "fail verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/header/wrong_header_timestamp.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    // fails with invalid commit value as does the rust implementation because of hashes instead of timestamp
    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidCommitValue)
  }

  "Wrong last block id" should "fail verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/header/wrong_last_block_id.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    // fails with invalid commit value as does the rust implementation because of hashes instead of last block id
    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidCommitValue)
  }
  "Wrong last commit hash" should "fail verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/header/wrong_last_commit_hash.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidCommitValue)
  }

  "Wrong next validator set hash" should "fail verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/header/wrong_next_valset_hash.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidNextValidatorSetHash)
  }

  "Wrong validator set hash" should "fail verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/header/wrong_valset_hash.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidValidatorSetHash)
  }

}
