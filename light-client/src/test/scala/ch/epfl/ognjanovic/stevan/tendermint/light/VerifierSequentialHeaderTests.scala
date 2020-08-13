package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.light.cases.SingleStepTestCase
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.rpc.Deserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.Failure
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import org.scalatest.flatspec.AnyFlatSpec

sealed class VerifierSequentialHeaderTests extends AnyFlatSpec with VerifierTests {

  implicit private val testCaseDeserializer: Deserializer[SingleStepTestCase] =
    new CirceDeserializer(SingleStepTestCase.decoder)

  private val votingPowerVerifier = VotingPowerVerifiers.defaultVotingPowerVerifier

  "Wrong chain id" should "fail verification" in {
    val (verifier, verifiedState, provider) =
      buildTest(VerifierTests.testCase("/single-step/sequential/header/wrong_chain_id.json"), votingPowerVerifier)

    val requestHeight = Height(2)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidHeader)
  }

  "Wrong header/commit height pair" should "fail verification" in {
    val (verifier, verifiedState, provider) =
      buildTest(VerifierTests.testCase("/single-step/sequential/header/wrong_header_height.json"), votingPowerVerifier)

    val requestHeight = Height(3)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    // fails with invalid commit value as does the rust implementation because of hashes instead of height
    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidCommitValue)
  }

  "Non monotonic header height value" should "trow an exception" in {
    val (verifier, verifiedState, provider) =
      buildTest(
        VerifierTests.testCase("/single-step/sequential/header/non_monotonic_header_height.json"),
        votingPowerVerifier)

    val requestHeight = Height(1)
    val nonMonotonicHeader = provider.lightBlock(requestHeight)

    assertThrows[IllegalArgumentException] {
      verifier.verify(verifiedState, nonMonotonicHeader)
    }
  }

  "Wrong header timestamp" should "fail verification" in {
    val (verifier, verifiedState, provider) =
      buildTest(
        VerifierTests.testCase("/single-step/sequential/header/wrong_header_timestamp.json"),
        votingPowerVerifier)

    val requestHeight = Height(2)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    // fails with invalid commit value as does the rust implementation because of hashes instead of timestamp
    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidCommitValue)
  }

  "Wrong last block id" should "fail verification" in {
    val (verifier, verifiedState, provider) =
      buildTest(VerifierTests.testCase("/single-step/sequential/header/wrong_last_block_id.json"), votingPowerVerifier)

    val requestHeight = Height(2)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    // fails with invalid commit value as does the rust implementation because of hashes instead of last block id
    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidCommitValue)
  }
  "Wrong last commit hash" should "fail verification" in {
    val (verifier, verifiedState, provider) =
      buildTest(
        VerifierTests.testCase("/single-step/sequential/header/wrong_last_commit_hash.json"),
        votingPowerVerifier)

    val requestHeight = Height(2)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidCommitValue)
  }

  "Wrong next validator set hash" should "fail verification" in {
    val (verifier, verifiedState, provider) =
      buildTest(
        VerifierTests.testCase("/single-step/sequential/header/wrong_next_valset_hash.json"),
        votingPowerVerifier)

    val requestHeight = Height(2)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidNextValidatorSetHash)
  }

  "Wrong validator set hash" should "fail verification" in {
    val (verifier, verifiedState, provider) =
      buildTest(VerifierTests.testCase("/single-step/sequential/header/wrong_valset_hash.json"), votingPowerVerifier)

    val requestHeight = Height(2)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    assert(result.isInstanceOf[Failure] && result.asInstanceOf[Failure].reason == InvalidValidatorSetHash)
  }

}
