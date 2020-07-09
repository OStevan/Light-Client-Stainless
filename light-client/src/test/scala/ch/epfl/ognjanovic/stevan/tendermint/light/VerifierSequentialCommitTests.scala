package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.light.cases.SingleStepTestCase
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.rpc.Deserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.{
  InsufficientCommitPower,
  InvalidCommit,
  InvalidCommitValue
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.{Failure, Success}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import org.scalatest.flatspec.AnyFlatSpec

sealed class VerifierSequentialCommitTests extends AnyFlatSpec with VerifierTests {

  implicit private val testCaseDeserializer: Deserializer[SingleStepTestCase] =
    new CirceDeserializer(SingleStepTestCase.decoder)

  private val votingPowerVerifier = VotingPowerVerifiers.defaultVotingPowerVerifier

  "Less than one third of nil votes" should "succeed for height 2" in {
    val (verifier, verifiedState, provider) =
      buildTest(
        VerifierTests.testCase("/single-step/sequential/commit/less_than_one_third_nil_votes.json"),
        votingPowerVerifier)

    val requestHeight = Height(2)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "More than two thirds of validators sign" should "succeed for height 2" in {
    val (verifier, verifiedState, provider) =
      buildTest(
        VerifierTests.testCase("/single-step/sequential/commit/more_than_two_third_vals_sign.json"),
        votingPowerVerifier)

    val requestHeight = Height(2)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Byzantine consensus not obtained on commit" should "succeed for height 2" in {
    val (verifier, verifiedState, provider) =
      buildTest(
        VerifierTests.testCase("/single-step/sequential/commit/one_third_vals_don't_sign.json"),
        votingPowerVerifier)

    val requestHeight = Height(2)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    assert(result.asInstanceOf[Failure].reason == InsufficientCommitPower)
  }

  "Commit of a wrong height" should "fail verification" in {
    val (verifier, verifiedState, provider) =
      buildTest(VerifierTests.testCase("/single-step/sequential/commit/wrong_commit_height.json"), votingPowerVerifier)

    val requestHeight = Height(2)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    assert(result.asInstanceOf[Failure].reason == InvalidCommit)
  }

  "Wrong header hash" should "fail verification" in {
    val (verifier, verifiedState, provider) =
      buildTest(VerifierTests.testCase("/single-step/sequential/commit/wrong_header_hash.json"), votingPowerVerifier)

    val requestHeight = Height(2)
    val result = verifier.verify(verifiedState, provider.lightBlock(requestHeight))

    assert(result.asInstanceOf[Failure].reason == InvalidCommitValue)
  }
}
