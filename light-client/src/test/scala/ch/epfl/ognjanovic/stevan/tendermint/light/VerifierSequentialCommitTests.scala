package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.hashing.HeaderHashers.DefaultHeaderHasher
import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.InvalidCommit
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes.{Failure, Success}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import org.scalatest.flatspec.AnyFlatSpec

sealed class VerifierSequentialCommitTests  extends AnyFlatSpec {

  "Less than one third of nil votes" should "succeed for height 2" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/commit/less_than_one_third_nil_votes.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    val headerHasher = new DefaultHeaderHasher(MerkleRoot.default)

    val hashValue = headerHasher.hashHeader(provider.lightBlock(requestHeight).header)

    assert(hashValue == provider.lightBlock(requestHeight).commit.blockId.bytes)
    assert(result == Success)
  }

  "More than two thirds of validators sign" should "succeed for height 2" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/commit/more_than_two_third_vals_sign.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result == Success)
  }

  "Byzantine consensus not obtained on commit" should "succeed for height 2" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/commit/one_third_vals_don't_sign.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result.asInstanceOf[Failure].reason == InvalidCommit)
  }

  "Commit of a wrong height" should "fail verification" in {
    val (verifier, trustedState, provider) = VerifierTests.deserializeSingleStepTestCase(
      "/single-step/sequential/commit/wrong_commit_height.json")

    val requestHeight = Height(2)
    val result = verifier.verify(trustedState, provider.lightBlock(requestHeight))

    assert(result.asInstanceOf[Failure].reason == InvalidCommit)
  }
}
